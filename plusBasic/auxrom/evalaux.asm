;=====================================================================================
; Evaluation externsion core routines
;=====================================================================================

;-------------------------------------------------------------------------
; Decode escaped string ending with NUL or "
;  Input: DE: Destination string buffer address
;         HL: Source string address
;  Output: A: Decoded string length
;         HL: Address after delimiter
;-------------------------------------------------------------------------
aux_escaped_string:
    push    de                    ; Stack = StrBuf, RtnAdr
.escape_loop
    ld      a,(hl)                ; A = NxtChr
    or      a                     ; If EOL
    jr      z,.done               ;   Finish up
    inc     hl                    ; Bump to NxtChr
    cp      '"'                   ; If double quote
    jr      z,.done               ;   Finish up
    cp      $5C
    jr      nz,.no_escape         ; If backslash
    ld      a,(hl)
    inc     hl                    ;   Eat it
    cp      'a'                   ;   If >= 'a'
    jr      c,.no_escape
    cp      'y'                   ;   or <= 'x'
    jr      c,.sequence           ;     Interpret escape sequence
.no_escape
    ld      (de),a          
    inc     de
    jr      .escape_loop
.sequence
    ld      c,a                   ; Save character
    ld      b,7                   ; ^G
    sub     'a'                   ;
    jr      z,.buff_it            ;   Ring the bell like Bob Dobbs
    inc     b                     ; ^H
    dec     a                     ; \b is for Backspace
    jr      z,.buff_it            ;
    ld      b,12                  ; ^L
    sub     'f'-'b'               ; If \f
    jr      z,.buff_it            ;   Feed the form
    sub     'n'-'f'               ; If \n
    jr      z,.crlf               ;   It's a new line
    inc     b                     ; ^M
    sub     'r'-'n'               ; If \r
    jr      z,.buff_it            ;   Make it clear screen
    ld      b,9                   ; ^I
    dec     a                     ; \s
    dec     a                     ; If \t
    jr      z,.buff_it            ;   Just a step to the right
    ld      b,11                  ; ^K
    dec     a                     ; \u
    dec     a                     ; If \v
    jr      z,.buff_it            ;   Make it clear
    dec     a                     ; Wumbo
    dec     a                     ; If \x
    jr      z,.hexit              ;   Do hex to ascii
    ld      b,c                   ; No map - write character
.buff_it
    ld      a,b                   ; Write mapped character to buffer
    jr      .no_escape            ; and loop
.crlf
    ld      a,13                  ; C/R
    ld      (de),a                ;  in the buffer
    inc     de
    ld      a,10                  ; L/F
    jr      .no_escape            ;  in and loop
.hexit
    call    aux_hex_to_byte
    jr      .no_escape
.done:
    xor     a
    ld      (de),a                ; Terminate string
    dec     hl                    ; Back up TxtPtr
    rst     CHRGET                ; Skip trailing spaces
    ex      (sp),hl               ; HL = StrBuf, Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; HL = StrPtr, DE = StrBuf
    xor     a
    sbc     hl,de                 ; HL = StrLen
    ld      a,l                   ; A = StrLen
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; Convert Binary String to Hexadecimal String
; Input: BC: Binary string length
;        DE: Binary string buffer
;        HL: Hex string buffer
; Output: BC: Hex string length
; Clobbered: A, DE
;-----------------------------------------------------------------------------
aux_asc_to_hex:
    ld    a,c                     ; A = BinLen
    add   a                       ; HexLen = BinLen * 2
    ret   z                       ; Return HexLen = 0 if BinLen = 0
    push  hl                      ; Stack = HexAdr, RtnAdr
    push  af                      ; Stack = HexLen, HexAdr, RtnAdr
    ld    b,c                     ; B = BinLen
.loop
    ld    a,(de)                  ; A = BinByt
    inc   de
    call  aux_byte_to_hex         ; Convert to Hex at (HL)
    djnz  .loop
    jr    _hex_done               ; Return BC = HexLen, HL = HexBuf

aux_byte_to_hex:
    ld      c,a
    rra
    rra
    rra
    rra
    call    .hex
    ld      a,c
.hex:
    and     $0f
    cp      10
    jr      c,.chr
    add     7
.chr:
    add     '0'
    ld      (hl),a
    inc     hl
    ret

;-----------------------------------------------------------------------------
; Convert Hexadecimal String to Binary String
; Input: BC: Hex string length
;        DE: Hex string buffer
;        HL: Binary string buffer
; Output: BC: Binary string length
; Clobbered: A, BC, DE
;-----------------------------------------------------------------------------
aux_hex_to_asc:
    ld      a,c                   ; A = AscLen / 2
    srl     a                     ; BinLen = AscLen / 2
    ret     c                     ; Return Carry if AscLen was Odd
    push    hl                    ; Stack = BinAdr, RtnAdr
    push    af                    ; Stack = BinLen, BinAdr, RtnAdr
    ex      de,hl                 ; HL = AscAdr, DE=BinAdr
    ld      b,a                   ; B = BinLen
.loop
    call    aux_hex_to_byte       ; A = Byte
    jp      c,discard2ret         ; If error, clean stack and return Carry set
    ld      (de),a
    inc     de
    djnz    .loop
_hex_done:
    pop     af
    ld      b,0
    ld      c,a                   ; BC = BinLen
    pop     hl                    ; HL = BufAdr
    ret

; Clobbers: C
aux_hex_to_byte:
    call    aux_get_hex           ; Get Hex Digit from Argument
    ret     c
    sla     a                     ; Shift to High Nybble
    sla     a
    sla     a
    sla     a
    ld      c,a                   ; and Save in C
    call    aux_get_hex           ; Get Next Hex Digit from Argument
    ret     c
    or      c                     ; Combine with High Nybble
    ret

aux_get_hex:
    ld      a,(hl)                ; Get Hex Digit
    inc     hl                    ; Bump Pointer
aux_cvt_hex:
    cp      '.'
    jr      nz,.not_dot           ; If dot
    add     '0'-'.'               ;   Convert to 0
.not_dot
    cp      ':'                   ; Test for Digit
    jr      nc,.not_digit         ; If A <= '9'
    sub     '0'                   ;   Convert Digit to Binary
    ret                           ;   Else Return
.not_digit
    and     $5F                   ; Convert to Upper Case
    sub     'A'-10                ; Make 'A' = 10
    ret     c                     ; Error if it was less than 'A'
    cp      16                    ;
    ccf                           ; Set Carry if > 15
    ret                           ;   Return

; In: DE = VarPtr
; CLobbers: A,BC
copy_literal_string:
    push    hl                    ; Stack = TxtPtr, RtnAdr
    push    de                    ; Stack = VarPtr, TxtPtr, RtnAdr
    ex      de,hl                 ; HL = VarPtr
    call    string_addr_len       ; DE = StrAdr, BC = StrLen
    jr      z,_no_copy            ; If StrLen = 0, pop VarPtr & TxtPtr and return
    ex      de,hl                 ; HL = StrAdr
    ld      DE,(STRSPC)           ; DE = Bottom of String Space
    rst     COMPAR                ; If StrAdr >= StrSpc
    jr      nc,_no_copy           ;   Pop VarPtr & TxtPtr and return
    pop     hl                    ; HL = VarPtr; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = VarPtr, TxtPtr, RtnAdr
    call    STRCPY                ; DE = DSCTMP
    pop     hl                    ; HL = VarPtr; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = VarPtr, TxtPtr, RtnAdr
    ld      de,DSCTMP
    call    VMOVE                 ; Copy (DSCTMP) to (VarPtr)
_no_copy:
    pop     de                    ; DE = VarPtr; Stack = TxtPtr, RtnAdr
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret
    
