;=====================================================================================
; buffer.asm - Routines to write to BASIC buffers
;=====================================================================================

;-----------------------------------------------------------------------------
; Read byte from BAS_BUFFR
; Input: DE = Address
; Output: C = Byte
;         DE = Coerced address + 1
; Clobbered: A
;-----------------------------------------------------------------------------
basbuf_read_byte:
    ld      a,BAS_BUFFR
;-----------------------------------------------------------------------------
; Read byte from BASIC buffer
; Input: A = Page
;       DE = Address
; Output: C = Byte
;         DE = Coerced address + 1
; Clobbered: A
;-----------------------------------------------------------------------------
buffer_read_byte:
    call    page_read_byte
    inc     de
    ret

;-----------------------------------------------------------------------------
; Read bytes from BAS_BUFFR
; Input: BC: Byte Count
;        DE: Destination address
;        HL: Source address (0-16383)
; Output: DE: Address after last byte written to destination
; Clobbered: AF,AF',BC,HL
;-----------------------------------------------------------------------------
basbuf_read_bytes:
    ld      a,BAS_BUFFR
;-----------------------------------------------------------------------------
; Read bytes from BASIC buffer
; Input: A: Page
;       BC: Byte Count
;       DE: Destination address
;       HL: Source address (0-16383)
; Output: DE: Address after last byte written to destination
; Clobbered: AF,AF',BC,HL
;-----------------------------------------------------------------------------
buffer_read_bytes:
    jp      page_fast_read_bytes

;-----------------------------------------------------------------------------
; Read word from BAS_BUFFR
; Input: DE = Address
; Output: BC = Word
;         DE = Coerced address + 2
; Clobbered: A
;-----------------------------------------------------------------------------
basbuf_read_word:
    ld      a,BAS_BUFFR
;-----------------------------------------------------------------------------
; Read word from BASIC buffer
; Input: DE = Address
; Output: BC = Word
;         DE = Coerced address + 2
; Clobbered: A
;-----------------------------------------------------------------------------
buffer_read_word:
    call    page_read_word
    inc     de
    inc     de
    ret

;-----------------------------------------------------------------------------
; Write byte to BAS_BUFFR
; Input: C = Byte
;        DE = Address
; Output: DE = Coerced address + 2
; Clobbered: A
;-----------------------------------------------------------------------------
basbuf_write_byte:
    ld      a,BAS_BUFFR
;-----------------------------------------------------------------------------
; Write byte to BASIC buffer
; Input: A = Page 
;        C = Byte
;       DE = Address
;       DE = Address
; Output: DE = Coerced address + 2
; Clobbered: A
;-----------------------------------------------------------------------------
buffer_write_byte:
    call    _buffer_write_init
    ld      a,c
    ld      (de),a                ; Write the byte
    inc     de
    jr      _buffer_write_done


;-----------------------------------------------------------------------------
; Write bytes to BAS_BUFFR
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
; Write bytes to BASIC buffer
; Input: A: Page 
;       BC: Byte Count
;        DE: Destination address (0-16383)
;        HL: Source address
; Output: DE: Coerced address after last byte written to destination
; Clobbered: AF,AF',BC,HL
;-----------------------------------------------------------------------------
buffer_write_bytes:
    call    _buffer_write_init
    ldir
_buffer_write_done:
    ex      af,af'                ; A = OldPg
    out     (IO_BANK3),a          ; Map old Page
    ret

;-----------------------------------------------------------------------------
; Write word to BAS_BUFFR
; Input: BC: Word
;        DE: Address
;        DE: Coerced address + 2
; Clobbered: A
;-----------------------------------------------------------------------------
basbuf_write_word:
    ld      a,BAS_BUFFR
;-----------------------------------------------------------------------------
; Write word to BASIC buffer
; Input: A: Page 
;       BC: Word
;        DE: Address
; Output: DE: Coerced address + 2
; Clobbered: A
;-----------------------------------------------------------------------------
buffer_write_word:
    call    _buffer_write_init
    ld      a,c
    ld      (de),a
    inc     de
    ld      a,b
    ld      (de),a
    inc     de
    jr      _buffer_write_done

_buffer_write_init:
    ex      af,af'                ; A' = WritePg
    in      a,(IO_BANK3)          ; A' = OldPg
    ex      af,af'                ; A = WritePg, A' = OldPg
    out     (IO_BANK3),a          ; Map WritePg into Bank 3
    ld      a,d                   ; Coerce Address
    or      $C0
    ld      d,a
    ret
