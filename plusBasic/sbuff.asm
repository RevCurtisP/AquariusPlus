;=============================================================================
; Extended BASIC Buffer Routines
;=============================================================================

;;To do: Change to use main memory string buffer

;-----------------------------------------------------------------------------
; Get Length of String in String Buffer
; Output: A,BC: String length, Flag set accordingly
;         DE: Address of STRBUFFLEN
;-----------------------------------------------------------------------------
sbuff_get_len:
    ld      de,STRBUFFLEN
_read_de_byte:
    ld      a,BAS_BUFFR
    call    page_read_byte
    ld      b,0
    ld      a,c
    or      a
    ret

;-----------------------------------------------------------------------------
; Update String Length
; Input: A = Amount to add
; Clobbers: A, BC, DE
;------------------------ -----------------------------------------------------
sbuff_update_len:
    push    de
    push    af
    call    sbuff_get_len
    pop     af
    add     a,c
    jr      c,.error
    ld      c,a
    call    sbuff_set_len
    pop     de
.error
    ret

;-----------------------------------------------------------------------------
; Initialize String Buffer
; Clobbers: A, C, DE
;-----------------------------------------------------------------------------
sbuff_init:
    ld      c,0
    jr      sbuff_set_len

;-----------------------------------------------------------------------------
; Set Length of String in String Buffer
; Input:  C: String length
;         DE: Addres of byte at length
; Clobbers: A,DE
;-----------------------------------------------------------------------------
sbuff_set_len:
    ld      de,STRBUFFLEN
_write_byte
    ld      a,BAS_BUFFR
    jp      page_write_byte

;-----------------------------------------------------------------------------
; Create BASIC string from buffer
; Returns: BC = StrLen
;          DE = StrAdr
;          HL = StrDsc
; Clobbers: A
;-----------------------------------------------------------------------------
sbuff_create_string:
    call    sbuff_get_len         ; Get text length
    jp      z,pop_ret_nullstr     ; If 0, return null string           
    call    STRINI                ; Create temporary string
    ld      c,a                   ;   HL = StrDsc, DE = StrAdr
    ld      b,0                   ; BC = StrLen
    push    hl                    ; Stack = StrDsc
    push    de                    ; Stack = StrAdr, StrDsc
    push    bc                    ; Stack = StrLen, StrAdr, StrDsc
read_bas_buffr:
    ld      a,BAS_BUFFR
    ld      hl,STRBUFF    
    call    page_read_bytes       ; Copy from Buffer to Temporary
    pop     bc                    ; BC = StrLen; Stack = StrAdr, StrDsc
    pop     de                    ; DE = StrAdr; Stack = StrDsc
    pop     hl                    ; HL = StrDsc
    ret

;-----------------------------------------------------------------------------
; Read Byte from String Buffer
; Input: E: Buffer Offset
; Output: A,BC: Byte read
;        DE: Address coerced to $C000-$FFFF
;-----------------------------------------------------------------------------
sbuff_read_byte:
    ld      d,high(STRBUFF)
    
_read_byte
    ld      a,BAS_BUFFR
    call    page_read_byte
    ld      b,0
    ld      a,c
    ret
    
;-----------------------------------------------------------------------------
; Write Byte to String Buffer
; Input: A: Byte to Write
; Clobbers: A
;-----------------------------------------------------------------------------
sbuff_write_byte:
    exx
    push    af
    call    sbuff_get_len         ; A = BufLen
    ld      d,high(STRBUFF)       
    ld      e,a                   ; Current Address in Buffer
    pop     af
    ld      c,a
    ld      a,BAS_BUFFR
    call    page_write_byte
    ld      a,1
    call    sbuff_update_len
    xor     a                     ; Clear Carry
    inc     a                     ; Clear Zero
.error
    exx
    ret

;-----------------------------------------------------------------------------
; Write Byte to String Buffer
; Input: A: Byte to Write
;        E: Offset
; Output: DE: Coerced Address + 1
; Clobbers: A
;-----------------------------------------------------------------------------
sbuff_write_byte_ofs:
    push    bc
    ld      d,high(STRBUFF)       
    ld      c,a
    ld      a,BAS_BUFFR
    call    page_write_byte
    inc     de
    xor     a                     ; Clear Carry
    inc     a                     ; Clear Zero
.error
    pop     bc
    ret


;-----------------------------------------------------------------------------
; Write Bytes to String Buffer
; Input: BC: Byte Count
;        DE: Source Address
; Output: Carry: Cleared if succesful, Set if overflow
; Clobbers: A, BC, DE, HL
;-----------------------------------------------------------------------------
sbuff_write_bytes:
    push    bc                    ; Stack = SrcLen
    ex      de,hl                 ; HL = SrcAdr
    call    sbuff_get_len         ; A = BufLen
    ld      d,high(STRBUFF)       
    ld      e,a                   ; Current Address in Buffer
    pop     bc                    ; BC = SrcLen
    ld      a,c                   ; A = SrcLen
    call    sbuff_update_len      ; BuffLen += SrcLen
    ret     c                     ; Error if Overflow
    ld      a,BAS_BUFFR           ; A = BASIC buffers page
    call    page_write_bytes
    xor     a                     ; Clear Carry
    inc     a                     ; Clear Zero
    ret

;-----------------------------------------------------------------------------
; Write Word to String Buffer
; Input: BC: Word to write
; Output: Carry: Cleared if succesful, Set if overflow
; Clobbers: A,DE
;-----------------------------------------------------------------------------
sbuff_write_de:
    ld      b,d
    ld      c,e
sbuff_write_bc:
    push    bc     
    call    sbuff_get_len
    ld      e,a                   ; DE = Address in buffer
    inc     d
    pop     bc
    ld      a,BAS_BUFFR
    call    page_write_word
    ret     c
    ld      a,2
    jp      sbuff_update_len

;-----------------------------------------------------------------------------
; Terminate string inbuffer
; Clobbers: A,BC,DE
;-----------------------------------------------------------------------------
sbuff_terminate:
    ld      de,STRBUFF
    ld      a,(STRBUFFLEN)
    add     a,e
    ld      e,a
    ld      c,0
    jp      page_write_byte

