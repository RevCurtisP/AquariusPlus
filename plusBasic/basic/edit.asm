;=====================================================================================
; Enhanced INPUT and Line Editor
;=====================================================================================

; ToDo: Import edit routine from editor.asm, integrate with _do_input routine.

con_input_string:
    ld      iy,_is_string
    jr    _do_input

con_input_hex:
    ld      iy,_is_hexdigit
    jr    _do_input

con_input_int:
    ld      iy,_is_digit
;-----------------------------------------------------------------------------
; Enhanced Input Core Routine
; Input: B: Minimum length
;        C: Maximum length
;        D: Column
;        E: Row
;       IY: Character validation routine
; Output: L: ENtry length
; Flags: Carry set if screen position out of bounds
;        Sign set if aborted (Ctrl-C)
;-----------------------------------------------------------------------------
_do_input:
    push    bc                    ; Stack = MinMax, RtnAdr
    call    get_strbuf_addr       ; HL = StrBuf
    ld      (BUFADR),hl           ; BUFADR = StrBuf
    pop     bc                    ; BC = MinMax, Stack = RtnAdr
    ld      hl,LINLEN
    ld      a,c                   ; A = MaxLen
    cp      37            
    ccf                           ; If MaxLen > 36
    ret     c                     ;   Return Carry Set
    add     a,d                   ; A = Column + MaxLen
    cp      (hl)
    ccf                           ; If end of field past end of line
    ret     c                     ;   Return Carry Set
    inc     e                     ; Bump Row to match LOCATE
    ld      a,e                   ; A = Row
    cp      25
    ccf                           ; If Row > 25
    ret     c                     ;   Return Carry Set
    ex      de,hl                 ; HL = ColRow
    call    aux_call_inline
    word    move_cursor           ; Move to input point
    ld      de,(BUFADR)           ; DE = BufPtr
    ld      l,0                   ; L = CurLen
_input_loop:
    call    key_read              ; Get keypress
    jr      z,_input_loop         ; If no key, try again
    cp      3                     ; If Ctrl-C
    jr      z,_abort_input        ;   Abort
    cp      24                    ; If Ctrl-X
    jr      z,_abort_input        ;   Abort
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
    jr      z,_input_error              ;   Beep and loop
    dec     l                     ; CurLen -= 1
    dec     de                    ; BufPtr -= 1
    ld      a,8                   ; 
    jr      _input_out            ; Back up cursor and loop
_finish_input:
    ld      (IEND_KEY),a          ; Save Key Value
    ld      a,l                   ; A = CurLen
    or      a
    jr      z,_return_abort
_validate_input:
    cp      b                     ; If CurLen >= MinLen
    jr      c,_input_error
    xor     a
    ld      (de),a                ; Terminate entry
    ld      hl,(BUFADR)           ; HL = BufAdr
    ret
_abort_input:
    ld      (IEND_KEY),a          ; Save Key Value
_return_abort:
    or      $FF                   ; Set Sign Bit
    ret

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
    rst     OUTCHR                ; Print character
    jr      _input_loop           ; and loop

