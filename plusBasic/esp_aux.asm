;=====================================================================================
; espx.asm - ESP32 routines in Aux ROM
;=====================================================================================

;-----------------------------------------------------------------------------
; espx_get_datetime - Read date and time into string buffer
; Input: HL = Buffer Address
; Sets: string_buff: Date and Time in format YYYYMMDDHHmmss
; Output:  E: String Length, DE = End of String, HL = Buffer Address
;-----------------------------------------------------------------------------
espx_get_datetime:
    ld      a,ESPCMD_DATETIME     ; Issue DATETIME command
    call    esp_cmd
    xor     a
    call    esp_send_byte         ; Response Type ($00)
    call    esp_get_result
    ret     m
    jp      espx_read_to_buff

;-----------------------------------------------------------------------------
; espx_get_gamectrl - Read date and time into string buffer
; Input: A: controller index
;       DE: buffer address
; Output: A: result code
;        BC: number of bytes read
;        DE: buffer address
; Result: 0: success
;        -1: ERR_NOT_FOUND
; Buffer: 0  (Signed) Left stick X
;         1  ( Signed )  Left stick Y
;         2  ( Signed )  Right stick X
;         3  ( Signed )  Right stick Y
;         4  (Unsigned)  Left trigger
;         5  (Unsigned)  Right trigger
;        6-7 (L-Endian)  Button Status
;             0  A
;             1  B
;             2  X
;             3  Y
;             4  View
;             5  Guide (Xbox button)
;             6  Menu
;             7  LS (Button in left stick)
;             8  RS (Button in right stick)
;             9  LB (Left shoulder button)
;            10  RB (Right shoulder button)
;            11  D-pad up
;            12  D-pad down
;            13  D-pad left
;            14  D-pad right
;            15  Share (Xbox Series S/X controller only)
;-----------------------------------------------------------------------------
get_gamectrl:                     ; For hump_aux table entry
espx_get_gamectrl:
    or      a
    jr      nz,ret_err_not_found

    push    af                    ; Stack = CtlIdx, RtnAdr
    ld      a,ESPCMD_GETGAMECTRL  ; Issue GETGAMECTRL command
    call    esp_cmd
    pop     af                    ; A = CtlIdx; Stack = RtnAdr
    call    esp_send_byte         ; Response Type ($00)
    call    esp_get_result
    ret     m
    push    de
    ld      bc,8
    call    esp_get_bytes
    pop     de
    ret

ret_err_not_found:
    ld      a,ERR_NOT_FOUND
    or      a
    ret

;-----------------------------------------------------------------------------
; esp_get_mouse - Read date and time into string buffer
; Output:  A: 0 if succesful, else error code
;         BC: X-position
;          E: Button State
;          D: Y-position
;          L: Wheel delta
;-----------------------------------------------------------------------------
espx_get_mouse:
    call    espn_get_mouse
    jp      page_restore_bank3

espn_get_mouse:
    ld      a,ESPCMD_GETMOUSE     ; Issue MOUSE command
    call    esp_cmd
    call    esp_get_byte
    or      a
    ret     m
    call    esp_get_long          ; BC = X, D = Y, E = Buttons
    xor     a
    call    esp_get_byte          ; A = Wheel Delta
    ld      l,a                   ; L = Wheel Delta
    xor     a                     ; Return success
    ret

;-----------------------------------------------------------------------------
; Read CR, LF, CR/LF, or null terminated string
;  Input: A: File descriptor
;        BC: Maximum line length
;        HL: String buffer address
; Output: A: Result
;        BC: String length
;         D: File descriptor
; Clobbered: A
;-----------------------------------------------------------------------------
espx_read_line:
    push    af
    ld      a,ESPCMD_READLINE
    call    esp_cmd
    pop     af
    push    af                    ; Stack = FilDsc, RtnAdr
    call    esp_send_byte         ; Send file descriptor
    call    esp_send_bc           ; Send maximum line length
    call    esp_get_result
    pop     de                    ; D = FilDsc; Stack = RtnAdr
    ret     m                     ; Return if error
    push    hl                    ; Stack = BufAdr, RtnAdr
    push    de                    ; Stack = FilDsc, BufAdr, RtnAdr
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
    pop     de                    ; D = FilDsc; Stack = BufAdr, RtnAdr
    pop     hl                    ; HL = BufAdr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; esp_read_to_buff - Read bytes from ESP to string buffer
; Input: HL: Address of String Buffer
; Output: E: String Length,
;        DE: Address of Terminator
;        HL: Buffer Address
; Clobbers: B
;-----------------------------------------------------------------------------
espx_read_to_buff:
    call    espx_read_buff
    jp      page_restore_bank3

espx_read_buff:
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
; esp_set_keymode - Set keyboard buffer mode
;  Input: A: Buffer Mode (KB_ENABLE | KB_ASCII | KB_REPEAT)
; Output: A: 0 if succesful, else error code
;-----------------------------------------------------------------------------
espx_set_keymode:
    push    a
    ld      a,ESPCMD_KEYMODE     ; Issue KEYMODE command
    call    esp_cmd
    pop     a
    call    esp_send_byte        ; Keyboard buffer mode
    call    esp_get_byte         ; Get result
    or      a
    jp      page_restore_bank3


;-----------------------------------------------------------------------------
; Write byte repeatedly
; Input:  E: byte to write
;         BC: number of times to write write it
; Output: BC: number of bytes actually written
; Clobbered registers: A, HL
;-----------------------------------------------------------------------------
espx_write_repbyte:
    ld      l,a

    ld      a, ESPCMD_WRITE
    call    esp_cmd

    ; Send file descriptor
    ld      a,l
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

