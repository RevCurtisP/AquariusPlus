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
    jp      esp_read_buff

;-----------------------------------------------------------------------------
; Close all files and directories
; Output: A: Result 
;-----------------------------------------------------------------------------
esp_close_all:
    ld      a, ESPCMD_CLOSEALL
    call    esp_cmd
    jp      esp_get_result 

;-----------------------------------------------------------------------------
; Read a byte from ESP to main memory
; Output: A: Result
;         C: 1 if succesful, else 0
;         B: Byte read (0 if none)
;-----------------------------------------------------------------------------
esp_read_byte:
    xor     a
esp_readc_byte:
    push    af
    ld      a, ESPCMD_READ
    call    esp_cmd               ; Issue read command
    pop     af
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
; Input: A: file descriptor (esp_readc_bytes)
;        BC: number of bytes to read
;        DE: destination address
; Output: A: result
;        BC: number of bytes actually read
;        DE: end address + 1
;         L: file descriptor
;-----------------------------------------------------------------------------
esp_read_bytes:
    xor     a
esp_readc_bytes:
    ld      l,a
    ld      a, ESPCMD_READ
    call    esp_cmd

    ; Send file descriptor
    ld      a,l
    call    esp_send_byte

    ; Send read size
    call    esp_send_bc
    
    ; Get result
    call    esp_get_result 
    ret     m

    ; Get number of bytes actual read
    call    esp_get_bc
;-----------------------------------------------------------------------------
; Read bytes from ESP requeat to main memory
; Input: BC: number of bytes to read
;        DE: destination address
; Output: A: 0 (success)
;        BC: number of bytes read
;        DE: end address + 1
; Note: Will hang if ESP sends less than BC bytes
;-----------------------------------------------------------------------------
esp_get_bytes:
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
    pop     bc
    ret

;-----------------------------------------------------------------------------
; Read bytes from ESP to paged memory
; Input: A: file descriptor
;       BC: number of bytes to read
;       DE: destination address
;        H: destination page
; Output: A: result code (clobbered if page error)
;        BC: number of bytes actually read
;        DE: next destination address 
;         H: new destination page
;         L: file descriptor
; Flags Set: Z if illegal page
;            C if page overflow
;            S if I/O error
;-----------------------------------------------------------------------------
xesp_read_paged:
    ld      l,a                   ; L = FilDsc
    ld      a,h                   ; A = Page
    jr      _read_paged
esp_read_paged:
    ld      l,a                   ; L = FilDsc
    ld      a,h                   ; A = Page
    call    page_check_write      ; If illegal page
    ret     z                     ;   Return error
_read_paged:
    call    page_map_bank3        ; Map page into bank 3
    call    page_coerce_de_addr
    
    ld      a, ESPCMD_READ        ; Send command
    call    esp_cmd
    ld      a,l                   ; Send file descriptor
    call    esp_send_byte       

    call    esp_send_bc           ; Send read size
    call    esp_get_result 
    jp      m,page_restore_bank3

    ; Get number of bytes actual read
    call    esp_get_bc
    push    bc                    ; Stack = BytCnt, RtnAdr
    dec     de
.loop:
    inc     de                    ; Update DE before checking BC
    ; Done reading? (BC=0)
    ld      a, b
    or      a, c
    jr      z, .done

    ld      a,d
    or      a,e
    jr      nz,.read_byte
    call    page_next_de_address
    ld      h,a                   ; H = New page
    jr      c,.error              ; Return if overflow
.read_byte
    in      a, (IO_ESPCTRL)
    rra
    jr      nc,.read_byte
    in      a, (IO_ESPDATA)
    ld      (de), a
    dec     bc                    
    jr      .loop

.done:   
    or      $7F                   ; Clear zero, carry, sign flags
    ld      a,0
.error
    pop     bc                    ; 
    jp      page_restore_bank3    ; Restore original page

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
esp_send_strdesc: 
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
; Read a byte from ESP to main memory
;  Input: C: Byte to write
; Output: A: Result
;         C: 1 if succesful, else 0
;-----------------------------------------------------------------------------
esp_write_byte:
    ld      a, ESPCMD_WRITE
    call    esp_cmd               ; Issue read command
    xor     a
    call    esp_send_byte         ; Send file descriptor
    ld      a,c
    push    bc                    
    push    af
    ld      bc,1
    call    esp_send_bc           ; Number of bytes to write
    pop     af
    call    esp_send_byte         ; Send byte
    call    esp_get_result 
    jp      m,.error
    call    esp_get_bc            ; Get number of bytes written
.error
    pop     bc
    ret

;-----------------------------------------------------------------------------
; Write bytes from main memory
; Input:  DE: source address
;         BC: number of bytes to write
; Output: DE: next address
;         BC: number of bytes actually written
; Clobbered registers: A, HL
;-----------------------------------------------------------------------------
esp_write_bytes:
    xor     a
esp_writec_bytes:
    push    af

    ld      a, ESPCMD_WRITE
    call    esp_cmd

    ; Send file descriptor
    pop     af
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
; Clobbered registers: A,AF',DE,IX
;-----------------------------------------------------------------------------
esp_write_paged:
    call    page_check_read       ; If illegal page       
    ret     z                     ;   Return error
    call    page_map_bank3        ; Map page into bank 3
    call    page_coerce_de_addr
    
    ld      a, ESPCMD_WRITE
    call    esp_cmd

    ; Send file descriptor
    xor     a
    call    esp_send_byte

    ; Send write size
    call    esp_send_bc

    ; Send bytes

    dec     de                    ; Backup for overflow check

.loop:
    ; Done sending? (BC=0)
    ld      a, b
    or      a, c
    jr      z, .done
    inc     de
    ld      a,d
    or      a,e
    call    z,page_next_de_address
    jr      c,.error              ; Return if overflow
    ld      a, (de)
    call    esp_send_byte
    dec     bc
    jr      .loop
.done
    call    esp_get_result 
    jp      m,page_restore_bank3
   
    call    esp_get_bc            ; Get number of bytes actual written
    ld      a,0
    or      $01                   ; Clear zero and carry flags
.error
    jp      page_restore_bank3    ; Restore originl page and return


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

esp_get_datetime:
    call    page_map_auxrom
    jp      espx_get_datetime

esp_get_mouse:
    call    page_map_auxrom
    jp      espx_get_mouse

esp_set_keymode:
    call    page_map_auxrom
    jp      espx_set_keymode
    
esp_read_line:
    call    page_map_auxrom
    call    espx_read_line
    jp      page_restore_bank3

esp_read_buff:
    call    page_map_auxrom
    call    espx_read_buff
    jp      page_restore_bank3

esp_write_repbyte:
    call    page_map_auxrom
    call    espx_write_repbyte
    jp      page_restore_bank3

