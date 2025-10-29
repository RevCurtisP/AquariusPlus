;=====================================================================================
; basbuf.asm - Routines that write to and read from page BAS_BUFFR
;=====================================================================================


;-----------------------------------------------------------------------------
; Initialize variables in page BAS_BUFFR
;-----------------------------------------------------------------------------
init_basbuf_vars:
    call    clear_history
    call    clear_mousedlt
    ret

;-----------------------------------------------------------------------------
; Write string to Autokey buffer
; Input: BC: String length
;        DE: String Address
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
autokey_write_buffer:
    ex      de,hl                 ; HL = StrAdr
    ld      de,BASBUF_BASE+WRTKEYBUF-1
    ld      (RESPTR),de           ; Set autokey address pointer to buffer.
    inc     de                    ; DE = BufAdr
    call    basbuf_write_bytes    ; C = 0 when done
    jp      basbuf_write_byte

;-----------------------------------------------------------------------------
; Write string to Function Key buffer
; Input: A: Function Key (0-15)
;       BC: String length
;       DE: String Address
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
fnkey_write_buffer:
    ex      de,hl                 ; HL = String Address
    ld      d,FKEYBASE/512        ; 011??000
    and     $0F                   ; 011??000 0 0000XXXX
    rla                           ; 011??000 0 000XXXX0
    rla                           ; 011??000 0 00XXXX00
    rla                           ; 011??000 0 0XXXX000
    rla                           ; 011??000 0 XXXX0000
    rla                           ; 011??000 X XXX00000
    rl      d                     ; 11??000X 0 XXX00000
    ld      e,a                   ; DE = Buffer Address
    call    basbuf_write_bytes
    inc     de
    jp      basbuf_write_byte


read_mousewdlt:
    ld      de,MOUSEWDLT
    call    basbuf_read_byte
    ld      a,c                   ; C = WheelDelt
    ret  

clear_mousedlt:
    xor     a
write_mousewdlt:
    ld      de,MOUSEWDLT
    ld      c,a
    jp      basbuf_write_byte

;----------------------------------------------------------------------------
; Get RUN arguments count
; Output: A, BC = # of Arguments (excluding filespec)
;         DE = Byte after ArgCount in ArgsBuffer
; Flags: Z if no arguments
; Clobbered: A
;----------------------------------------------------------------------------
runarg_count:
    ld      de,RUNARGS
    call    basbuf_read_word
    ld      a,c
    ret

;----------------------------------------------------------------------------
; Get RUN argument
; Input: BC = Argument# (0 = filespec)
;        HL = Buffer Address
; Output: DE = Address of string terminator
; Flags: Carry set if illegal argument number
; Clobbered: AF,AF',BC,HL
;----------------------------------------------------------------------------
; ToDo: Parse args after filespec
runarg_get:
    push    bc                    ; Stack = ArgNum, RtnAdr
    call    runarg_count          ; A = ArgCnt, DE = ArgPtr
    pop     bc                    ; C = ArgNum; Stack = RtnAdr
    inc     bc                    ; Bump ArgNum for compare
    cp      c                     ; If ArgCnt < ArgNum
    ret     c                     ;   Return Carry Set
.get_arg
    call    basbuf_read_word      ; BC = ArgLen, DE = ArgTxt
    push    bc                    ; Stack = ArgLen, TxtPtr, RtnAdr
    ex      de,hl                 ; HL = ArgTxt, DE = BufAdt
    pop     bc                    ; BC = ArgLen; Stack = TxtPtr, RtnAdr
    call    basbuf_read_bytes     ; Copy ARG to Buffer
    xor     a
    ld      (de),a                ; Terminate string
    ret

clear_history:
    ld      de,LINHSTPTR
    ld      bc,LINHSTTOP
    jp      basbuf_write_word

; On entry: B = LinLen, HL = BufAdr+1
; Carry Set if firsst character is digit
write_history:
    ret     c                     ; Return if line number
    push    hl                    ; Stack = BufPtr, RtnAdr
    ld      c,b
    ld      b,0                   ; BC = LinLen
    push    bc                    ; Stack = LinLen, BufPtr, RtnAdr
    dec     hl                    ; HL = BufAdr
    ld      de,PRVDIRLIN          ; DE = PrvBuf
    call    basbuf_write_bytes    ; Copy Input Buffer to PrvLineBuf
    pop     bc                    ; BC = LinLen; Stack = LinLen, BufPtr, RtnAdr
    call    _read_hist_ptr        ; HL = HstPtr
    ld      de,LINHSTTOP+1
    rst     COMPAR
    jr      nc,.done              ; If HstPtr <= HstTop
    sbc     hl,bc                 ;   HL = NewPtr
    ld      de,LINHSTBOT          
    rst     COMPAR                ; 
    jr      c,.done               ;   If NewPtr >= HstBtm
    call    _write_hist_ptr       ;   Save NewPtr
    ex      de,hl                 ;     DE = NewPtr
    call    get_linbuf_hl         ;     HL = BufAdr
    call    basbuf_write_bytes    ;     Copy Buffer to History
.done
    pop     hl                    ; HL = BufPtr; Stack = RtnAdr
    ret


_read_hist_ptr:
    ld      de,LINHSTPTR
_read_word_hl:
    push    bc
    call    basbuf_read_word      ; BC = HstPtr
    ld      h,b
    ld      l,c
    pop     bc
    ret
    
_write_hist_ptr:
    ld      de,LINHSTPTR
_write_word_hl:
    push    bc
    ld      b,h
    ld      c,l
    call    basbuf_write_word      ; BC = HstPtr
    pop     bc
    ret