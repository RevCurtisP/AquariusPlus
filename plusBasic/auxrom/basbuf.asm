;=====================================================================================
; basbuf.asm - Routines that write to and read from page BAS_BUFFR
;=====================================================================================

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
