;-----------------------------------------------------------------------------
; basbuf.asm - Basic Buffers read/write and associated routines
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; Read byte from BASIC buffer
; Input: DE = Address
; Output: C = Byte
;         DE = Coerced address + 1
; Clobbered: A
;-----------------------------------------------------------------------------
basbuf_read_byte:
    ld      a,BAS_BUFFR
    call    page_read_byte
    inc     de
    ret

;-----------------------------------------------------------------------------
; Read bytes from BASIC buffer
; Input: BC: Byte Count
;        DE: Destination address
;        HL: Source address (0-16383)
; Output: DE: Address after last byte written to destination
; Clobbered: AF,AF',BC,HL
;-----------------------------------------------------------------------------
basbuf_read_bytes:
    ld      a,BAS_BUFFR
    jp      page_fast_read_bytes

;-----------------------------------------------------------------------------
; Read word from BASIC buffer
; Input: DE = Address
; Output: BC = Word
;         DE = Coerced address + 2
; Clobbered: A
;-----------------------------------------------------------------------------
basbuf_read_word:
    ld      a,BAS_BUFFR
    call    page_read_word
    inc     de
    inc     de
    ret

;-----------------------------------------------------------------------------
; Write bytes from BASIC buffer
; Input: BC: Byte Count
;        DE: Destination address (0-16383)
;        HL: Source address
; Output: DE: Coerced address after last byte written to destination
; Clobbered: AF,AF',BC,HL
;-----------------------------------------------------------------------------
basbuf_write_bytes:
    ld      a,b
    or      c
    ret     z
    ld      a,BAS_BUFFR
    jp      buffer_write_bytes

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
;-----------------------------------------------------------------------------
; Write byte to BASIC buffer
; Input: C = Byte
;        DE = Address
; Output: DE = Coerced address + 2
; Clobbered: A
;-----------------------------------------------------------------------------
basbuf_write_byte:
    ld      a,BAS_BUFFR
    call    buffer_write_byte
    inc     de
    ret

;-----------------------------------------------------------------------------
; Write word to BASIC buffer
; Input: BC = Word
;        DE = Address
; Output: DE = Coerced address + 2
; Clobbered: A
;-----------------------------------------------------------------------------
basbuf_write_word:
    ld      a,BAS_BUFFR
    call    buffer_write_word
    inc     de
    inc     de
    ret

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
;        HL = Destination address
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
    ret     c                     ;   and Return
    ld      de,RUNARGS+2
    call    basbuf_read_word      ; BC = ArgLen, DE = ArgTxt
    push    bc                    ; Stack = ArgLen, TxtPtr, RtnAdr
    ex      de,hl                 ; HL = ArgTxt, DE = StrBuf
    pop     bc                    ; BC = ArgLen; Stack = TxtPtr, RtnAdr
    call    basbuf_read_bytes     ; Copy ARG to StrBuf
    xor     a
    ld      (de),a                ; Terminate string
    ret

