;=====================================================================================
; esp.asm - ESP32 routines
;=====================================================================================

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
; Load FPGA Core 
; Input:  DE: path string address
;         BC: path string length
;-----------------------------------------------------------------------------
esp_load_fpga:
    ld      a,ESPCMD_LOADFPGA
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
; Get first result byte
; Output: A: Result, negativ if error
;-----------------------------------------------------------------------------
esp_get_result:
    call    esp_get_byte
    or      a
    ret

;-----------------------------------------------------------------------------
; Get system version string
;  Input: HL: String buffer address
; Output: BC: String length
; Clobbered: A
;-----------------------------------------------------------------------------
esp_get_version:
    ld      a, ESPCMD_VERSION
    call    esp_cmd

;-----------------------------------------------------------------------------
; Read null-terminated string
;  Input: HL: String buffer address
; Output: BC: String length
; Clobbered: A
;-----------------------------------------------------------------------------
esp_read_string:
    push    hl
.loop
    ld      bc,0
    call    esp_get_byte
    ld      (hl),a
    or      a
    jr      z,pop_hl_ret
    inc     hl
    inc     bc
    jr      .loop
pop_hl_ret:
    pop     hl
    ret

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
    ld      (de),a                ; Store in Buffer
    or      a
    jr      z,.done               ; Return if end of String
    inc     de
    djnz    .loop     
.done
    ex      de,hl                 ; HL = Terminator Address, DE = Buffer Address
    sbc     hl,de                 ; HL = Length
    ex      de,hl                 ; DE = Length, HL = Buffer Address
    pop     af                    ; Restore Result
    ret

;-----------------------------------------------------------------------------
; Read a byte from ESP to main memory
; Output: A: Result
;         C: 1 if succesful, else 0
;         B: Byte read (0 if none)
;-----------------------------------------------------------------------------
esp_read_byte:
    ld      a, ESPCMD_READ
    call    esp_cmd               ; Issue read command
    xor     a
    call    esp_send_byte         ; Send file descriptor
    ld      bc,1
    call    esp_send_bc           ; Number of bytes to read
    call    esp_get_result 
    ret     m                     ; Return if error
    call    esp_get_bc            ; Get number of bytes actual read
    ld      a,c
    or      a
    ret     z                     ; Return if EOF
    call    esp_get_byte          ; Read the byte
    ld      b,a                   ; Return it in B
    xor     a                     ; Return No Error
    ret


;-----------------------------------------------------------------------------
; Read bytes from ESP to main memory
; Input: BC: number of bytes to read
;        DE: destination address
; Output: A: Result
;        BC: number of bytes actually read
;     DE,HL: next address (start address if no bytes read)
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
    ret     m

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
; Input: A: page
;       BC: number of bytes to read
;       DE: destination address
; Output: A: result code
;        BC: number of bytes actually read
;     DE,HL: next address (start address if no bytes read)
; Flags Set: Z if llegal page
;            C if page overflow
;            S if I/O error
;-----------------------------------------------------------------------------
esp_read_paged:
    call    page_set4write_coerce
    jp      z,page_restore_plus   ; Return if illegal page

    ld      a, ESPCMD_READ
    call    esp_cmd

    ; Send file descriptor
    xor     a
    call    esp_send_byte

    ; Send read size
    call    esp_send_bc
    
    ; Get result
    call    esp_get_result 
    ret     m                     ; Return if error

    ; Get number of bytes actual read
    call    esp_get_bc

    push    bc                    ; Stack = BytCnt, RtnAdr

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
    call    page_next_de_address
    jr      c,.error              ; Return if overflow
.read_byte
    call    esp_get_byte
    ld      (de), a
    dec     bc                    
    jr      .loop

.done:   
    or      $7F                   ; Clear zero, carry, sign flags
    ld      a,0
    ld      h,d                   ; Return end address in DE and HL
    ld      l,e
.error
    pop     bc                    ; 
    jp      page_restore_plus     ; Restore original page

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
; Send string pointed to by string descriptor
; Input: HL: String descriptor address
;-----------------------------------------------------------------------------
esp__send_strdesc: 
    push    af
    push    bc
    push    de
    ld      a,h                   ; If HL is 0
    or      l
    jr      z,.terminate          ; Send Null Terminator
    call    string_addr_len       ; Get Text Address in DE and Length in BC
    call    esp_send_bytes        ; Send Command String  
.terminate
    xor     a 
    call    esp_send_byte         ; Send String Terminator
    pop     de
    pop     bc
    pop     af
    ret

;-----------------------------------------------------------------------------
; Send String from String Descriptor
; Input:  HL: String Descriptor Address
; Output: BC: number of bytes written
;         DE: end address+1
; Clobbered: A
;-----------------------------------------------------------------------------
esp_send_strdesc: 
    ld      a,h                   
    or      l                     ; If NULL StringDesc address
    jp      z,esp_send_byte       ;   Send Null Terminator
    call    string_addr_len       ; Get Text Address in DE and Length in BC
 
;-----------------------------------------------------------------------------
; Send String
; Input:  DE: string address
;         BC: string length
; Output: BC: number of bytes written
;         DE: end address+1
; Clobbered: A
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
    ret     m

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
    ret     m

    ; Get number of bytes actual written
    call    esp_get_bc

    ret

;-----------------------------------------------------------------------------
; Send bytes from main memory to ESP32
; Input:  DE: start address
;         BC: number of bytes to write
; Output: BC: number of bytes actually written
;         DE: end address+1
; Clobbers: A
;-----------------------------------------------------------------------------
esp_send_bytes:
    push    bc
.loop:
    ld      a, b
    or      a, c                  ; If BC = 0
    jr      z, .done              ;   Finish up

    ld      a, (de)
    call    esp_send_byte
    inc     de
    dec     bc
    jr      .loop

.done:
    pop     bc                    ; Return Bytes written
    ret

;-----------------------------------------------------------------------------
; Write bytes from paged memory
; Input:   A: page
;         DE: source address
;         BC: number of bytes to write
; Output: BC: number of bytes actually written
;
; Clobbered registers: A, DE
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
    call    page_next_de_address
    jr      c,.error              ; Return if overflow
.not_end
    dec     bc
    jr      .loop

.done:

    ; Get result
    call    esp_get_result 
    ret     m

    ; Get number of bytes actual written
    call    esp_get_bc

    ld      h,a
    or      $01                   ; Clear zero and carry flags
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
; Input: A: byte
;-----------------------------------------------------------------------------
esp_send_byte:
    push    af
.wait:
    in      a, (IO_ESPCTRL)
    and     a, 2
    jr      nz, .wait
    pop     af
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
;          L: Wheel delta
;-----------------------------------------------------------------------------
esp_get_mouse:
    ld      a,ESPCMD_GETMOUSE     ; Issue MOUSE command
    call    esp_cmd
    call    esp_get_byte
    or      a
    ret     m                     
    call    esp_get_long          ; BC = X, D = Y, E = Buttons
    xor     a
;   call    esp_get_byte          ; A = Wheel Delta
    ld      l,a                   ; L = Wheel Delta
    xor     a                     ; Return success
    ret

;-----------------------------------------------------------------------------
; esp_set_keymode - Set keyboard buffer mode
;  Input: A: Buffer Mode (KB_ENABLE | KB_SCANCODE | KB_REPEAT) 
; Output: A: 0 if succesful, else error code
;-----------------------------------------------------------------------------
esp_set_keymode:
    push    a
    ld      a,ESPCMD_KEYMODE     ; Issue DATETIME command
    call    esp_cmd
    pop     a
    call    esp_send_byte        ; Keyboard buffer mode 
    call    esp_get_byte         ; Get result
    or      a
    ret
    
esp_read_line:
    call    page_map_aux
    call    esp_aux_read_line
    jp      page_restore_plus

