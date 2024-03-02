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
; Read bytes from BASIC buffer
; Input: BC: Byte Count
;        DE: Destination address
;        HL: Source address (0-16383)
; Output: DE: Coerced address after last byte written to destination
; Clobbered: AF,AF',BC,HL
;-----------------------------------------------------------------------------
basbuf_write_bytes:
    ld      a,BAS_BUFFR
    jp      page_fast_write_bytes

;-----------------------------------------------------------------------------
; Write word to BASIC buffer
; Input: BC = Word
;        DE = Address
; Output: DE = Coerced address + 2
; Clobbered: A
;-----------------------------------------------------------------------------
basbuf_write_byte:
    ld      a,BAS_BUFFR
    call    page_write_byte        
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
    call    page_write_word
    inc     de
    inc     de
    ret

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

