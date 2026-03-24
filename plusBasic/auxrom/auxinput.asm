;=====================================================================================
; Enhanced INPUT and Line Editor
;=====================================================================================

; ToDo: Import edit routine from editor.asm, integrate with _do_input routine.

; Do: INPUT (col,row),minlen,maxlen,INT var
; Input: A: Type - INT, FLOAT*, HEX*, STR*, 
;        B: MinLen, C: MaxLen, D = Column, E = Row, HL = VarPtr
bas_input:
    ld      (INPVAR),hl
    ld      (INPMAX),bc
    ld      (INPROW),de
    call    get_strbuf_addr
    ld      (INPBUF),hl
    cp      INTTK
    jr      z,bas_input_int
    cp      FLOATK
    jp      z,bas_input_float
    cp      HEXTK
    jp      z,bas_input_hex
    cp      STRTK
    jp      z,bas_input_string
    jp      ERRTO                 ; Invalid mode error

bas_input_string:
    call    _set_buff_string      ; A = StrLen
    ld      iy,_is_string
    call    _input_string         ; C = StrLen, HL = StrAdr
    jp      m,_abort
    ex      de,hl                 ; DE = StrAdr
    call    STRAD2                ; HL = DscPtr
    ex      de,hl                 ; DE = DscPtr
    ld      hl,(INPVAR)
    push    hl                    ; Stack = Dummy, RtnAdr
    push    hl                    ; Stack = VarPtr, Dummy, RtnAdr
    jp      INBUFD

bas_input_hex:
    jp      IMERR
    call    _set_buff_hex
    ld      iy,_is_hexdigit       ; C = StrLen, HL = StrAdr
    call    _input_num
    ret     m
    call    aux_eval_hex          ; CDE = Long
    jp      c,OVERR
    call    aux_float_cde
    jp      c,FCERR
    ret

bas_input_float:
    jp      IMERR
    ld      iy,_is_floatchar
    jr      _do_num

bas_input_int:
    call    _set_buff_int
    ld      iy,_is_digit
_do_num:
    call    _input_num            ; C = StrLen, HL = StrAdr    
 set_num:
    jp      m,_abort
    jp      c,FCERR
    call    z,_out_zero
    call    FIN                   ; FACC = Entry
_ret_num:
    ld      hl,(INPVAR)
    jp      MOVMF                 ; Copy FACC to Variable and Return
    
    
_input_string:
    call    CHKSTR                ; Type mismatch error if not string var
    jr      _do_input
_input_num:    
    call    CHKNUM                ; Type mismatch error if not numeric var
;-----------------------------------------------------------------------------
; Enhanced Input Core Routine
; Input: B: Minimum length
;        C: Maximum length
;        D: Column
;        E: Row
;       IY: Character validation routine
; Output: A: Entry Length
;         HL: Buffer Address
; Flags: Carry set if screen position out of bounds
;        Sign set if aborted (Ctrl-C)
;-----------------------------------------------------------------------------
_do_input:
    ld      hl,LINLEN
    ld      bc,(INPMAX)
    inc     e                     ; Bump Row to match LOCATE
    ld      de,(INPROW)
    ld      a,c
    cp      37            
    ccf                           ; If MaxLen > 36
    ret     c                     ;   Return Carry Set
    add     a,d                   ; A = Column + MaxLen
    cp      (hl)
    ccf                           ; If end of field past end of line
    ret     c                     ;   Return Carry Set
    ld      a,e                   ; A = Row
    cp      24
    ccf                           ; If Row > 23
    ret     c                     ;   Return Carry Set
    ex      de,hl                 ; HL = ColRow
    call    move_cursor
    call    _disp_entry
    ld      hl,(INPLEN)           ; L = CurLen
    ld      de,(INPBUF)           ; DE = INPBUF
    add     hl,de                 ; HL = BufPtr
    ex      de,hl                 ; DE = BufPtr
    ld      hl,(INPLEN)
    ld      bc,(INPMAX)
_input_loop:
    call    key_read              ; Get keypress
    jr      z,_input_loop
    cp      3                     ; If Ctrl-C
    jr      z,_abort_input        ;   Abort
    cp      24                    ; If Ctrl-X
    jr      z,_delete_entry
    cp      9                     ; If Tab
    jr      z,_finish_input       ;   Try to leave
    cp      140                   ; If BackTab
    jr      z,_finish_input       ;   Try to leave
    cp      143                   ; If Up
    jr      z,_finish_input       ;   Try to leave
    cp      159                   ; If Down
    jr      z,_finish_input       ;   Try to leave
    cp      13                    ; If Enter
    jr      z,_finish_input       ;   Try to leave
    cp      8                     ; If Backspace
    jr      z,_back_up            ;   Try to back up  
    jp      (iy)                  ; Check character
_good_char:
    ex      af,af'                ; A' = KeyASC
    ld      a,l                   ; A = CurLen
    cp      c                     ; If CurLen >= MaxLen
    jr      nc,_input_error       ;   Beep and loop
    ex      af,af'                ; A = KeyASC
    ld      (de),a                ; Put in buffer
    inc     l                     ; CurLen += 1
    inc     de                    ; BufPtr += 1
    jr      _input_out            ; Print it and loop
_back_up:
    ld      a,l                   ; A = CurLen
    or      a                     ; If CurLen = 0
    jr      z,_input_error        ;   Beep and loop
    dec     l                     ; CurLen -= 1
    dec     de                    ; BufPtr -= 1
    ld      a,8                   ; 
    jr      _input_out            ; Back up cursor and loop
_finish_input:
    ld      (IEND_KEY),a          ; Save Key Value
    call    _trim_entry
    ld      a,l
_validate_input:
    cp      b                     ; If CurLen >= MinLen
    jr      c,_input_error
    xor     a
    ld      (de),a                ; Terminate entry
    ld      a,l
    ld      hl,(INPBUF)           ; HL = INPBUF
    or      a
    ret
_abort_input:
    ld      (IEND_KEY),a          ; Save Key Value
_return_abort:
    or      $FF                   ; Set Sign Bit
    ret

_trim_entry:
    inc     l
.tloop
    dec     l
    ret     z
    dec     de
    ld      a,(de)
    cp      ' '
    jr      nz,.done
    ld      a,8
    call    _ttyout
    jr      .tloop
.done
    inc     de
    ret

_delete_entry:
    ld      a,8                   ; A = BkSpc
    inc     l                     ; Bump for DEC
.loop
    dec     l
    jr      z,_input_loop
    call    _ttyout
    jr      .loop

_is_string:
    cp      32                    ; If control character
    jr      c,_input_error        ;   Reject it
    cp      $A0                   ; If Latin-1 character
    jr      nc,_good_char         ;   Process it
    cp      $7F                   ; If DEL - $9F
    jr      nc,_input_error       ;   Reject it
    jr      _good_char            ; Else process it

; Returns carry set if invalid character
_is_hexdigit:
    cp      'a'
    jr      c,.notlower           ; If lower case letter
    and     $5F                   ;   Convert to lower case
.notlower
    cp      'G'                   ; If > 'F'
    jr      nc,_input_error       ;   Reject it
    cp      'A'                   ; If >=  'A'
    jr      nc,_good_char         ;   Process it
_is_digit:
    cp      '0'                   ; If < '0'
    jr      c,_input_error        ;   Reject it
    cp      ':'                   ; If <= '9'
    jr      c,_good_char          ;   Process it
_input_error:
    ld      a,7                   ; Ring the bell
_input_out:
    call    _ttyout               ; Print character
    jp      _input_loop           ; and loop

; ToDo: Finish This
_is_floatchar:
    ld      d,a                   ; D = Chr
    push    hl
    cp      '-'
    jr      z,.check_minus
    cp      '.'
    jr      z,.check_dot
    
.check_dot
    inc     e
    ld      hl,(INPBUF)
.dot_loop
    dec     e
    jp      z,_good_char
    cp      (hl)
    jr      z,_input_error
    inc     hl
    jr      .dot_loop
.check_minus
    ld      a,l
    or      a
.check_char
    pop     hl
    jr      nz,_input_error
    ld      a,d
    ld      l,e
    jp      _good_char

_set_buff_int:
    xor     a                     ; Z = Integer
    ld      hl,(INPVAR)           ; HL = VarPtr
    call    MOVFM                 ; Copy Variable to FACC
    call    z,INT                 ; Convert to Integer
    rst     FSIGN                 ; Set Z if 0
    jr      nz,.fout_buff
    ld      a,(INPMAX)
    ld      bc,0
    ld      de,(INPBUF)
    call    _pad_num
    ld      hl,BUF
    ld      (hl),'0'
    ret
.fout_buff
    call    FOUT
    ld      hl,FBUFFR+1
    ld      a,(hl)
    cp      ' '
    jr      nz,_copy_fbuffr
    inc     hl
_copy_fbuffr:
    ld      bc,0
    ld      de,(INPBUF)
.loop
    ld      a,(hl)
    or      a
    jr      z,_pad_num
    ld      (de),a
    inc     hl
    inc     de
    inc     bc
    jr      .loop
_pad_num:
    ld      (INPLEN),bc
    ld      a,(INPMAX)
    sub     c                     ; A = StrLen - MaxLen
    jp      c,OVERR               ; String too long error if StrLen > MaxLen 
    jr      _pad_buff

; Output: A, HL = StrLen
_set_buff_hex:
    ld      hl,(INPVAR)           ; HL = VarPtr
    call    MOVFM                 ; Copy Variable to FACC
    call    FRC_LONG              ; CDE = Long`
    call    bas_hex_long
    jr      _copy_fbuffr
    
_set_buff_string:
    ld      hl,(INPVAR)
    call    string_addr_len       ; DE = StrAdr, A, BC = StrLen
    ex      de,hl                 ; HL = StrAdr
    ld      de,(INPBUF)           ; DE = BufAdr`
    ld      (INPLEN),bc
    ld      a,(INPMAX)
    jr      z,_pad_buff
    sub     c                     ; A = StrLen - MaxLen
    jp      c,SLERR               ; String too long error if StrLen > MaxLen 
    ldir
_pad_buff:
    ld      (INPPAD),a
    or      a                     ; If StrLen - MaxLen
    jr      z,_copy_buf
    ld      b,a
    ld      a,' '
.loop
    ld      (de),a
    inc     de
    djnz    .loop
_copy_buf:  
    ld      hl,(INPBUF)
    ld      de,BUF
    ld      bc,(INPMAX)
    ld      b,0
    ldir
    ret

; Input: C = MaxLen
; Clobbers: A, DE
_disp_entry:
    push    iy
;    xor     a                    ; Turn off cursor
;    call    set_cursor_mode      ; ToDo: Move this to AuxROM
    ld      de,(INPBUF)
    call    _out_buff
    ld      a,(INPPAD)
    or      a
    jr      z,.done
    ld      b,a
    ld      a,8
.bloop
    call    _ttyout
    djnz    .bloop
.done
;    call    set_cursor_on
    pop     iy
    ret

_out_buff
    ld      a,(INPMAX)
    ld      b,a
.ploop
    ld      a,(de)
    inc     de
    call    _ttyout
    djnz    .ploop
    ret

_abort:
    ld      hl,(INPROW)
    call    move_cursor
    ld      de,BUF
    call    _out_buff
    ret

aux_float_cde:
    ld      a,c
    ld      b,$98                 ; Exponent = 2^24
    jp      FLOATR                ; Float It

; Input: HL = StrPtr
; Output: CDE = Long
aux_eval_hex:
    ld      bc,0
    ld      de,0
.hex_loop
    dec     b
    ret     z
    ld      a,(hl)
    inc     hl
    cp      'a'                   ; If Char >= 'a'
    jr      c,.hex_byte
    cp      '{'                   ; and <= 'z'
    jr      nc,.hex_byte
    and     $5F                   ;   Convert to uppercase
.hex_byte
    sub     '0'                   ; Convert Digit to Byte
    ccf
    ret     nc                    ;   Return Carry Clear if Char < '0'
    cp      ':'-'0'
    jr      c,.do_digit           ; If Char > '9'
    sub     'A'-':'               ;   Convert Hex Digit to byte
    cp      16                    ;   If  Byte > 15
    ret     nc                    ;     Return Carry Clear
.do_digit
    ex      af,af'                ; A' = HexByte
    ld      a,4                   ; Shift DE Left 4 bits
.sla_loop
    sla     e
    rl      d
    rl      c
    ret     c                     ; Return Carry Set if overflow
    dec     a
    jr      nz,.sla_loop
    ex      af,af'                ; A = HexByte
    or      e                     ; Put into low nybble
    ld      e,a
    jr      .hex_loop             ; Look for Next Hex Digit

_out_zero:    
    ld      a,'0'
_ttyout:
    push    iy
    call    TTYOUT
    pop     iy
    ret