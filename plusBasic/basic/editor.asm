;=====================================================================================
; Enhanced direct mode Line Editor
;=====================================================================================
; ToDo: Debug running from aux_rom

; To test:
; CALL $201E

;-----------------------------------------------------------------------------
; Line editor. Assumes that cursor is at end of line.
;  Input: C: Line length
;        HL: Buffer address
; Output: B: Position in line
;         C: Line length
;        DE: Position address 
;        HL: Buffer address
;-----------------------------------------------------------------------------
edit:
    push    hl                    ; Stack = BufAdr, RtnAdr
    add     hl,bc                 ; HL = CurAdr (End of Line)
    ld      b,c                   ; B = CurPos = LinLen
    call    _res_char             ; Restore character under cursor
    xor     a
_tloop
    ld      (hl),a                ; Make sure it's terminated
_loop
    call    screen_invert_cursor  ; Display cursor
.wait
    call    key_read_ascii        ; Wait for Key
    jr      z,.wait
    call    screen_invert_cursor  ; Display cursor
    cp      ' '                   ; If Control Key
    jr      c,_ctrlkey            ;   Process it
    cp      $7F                   ; If Cursor Key
    jr      nc,_cursor_key        ;   Process it
    inc     b                     ; Bump position
    jr      z,_toofar             ; Error if 256
    call    _out_char             ; Print character to screen
    ld      (hl),a                ; Stuff in buffer
    inc     hl                    ; Bump buffer pointer
    ld      a,c
    sub     b                     
    jr      nc,_loop              ; If LinLen < CurPos
    ld      c,b                   ;   LinLen = CurPos
    jr      _loop

_toofar
    dec     b                     ; Back up to 255
_beep    
    ld      a,'G'-64              ; 
    call    _out_char
    call    key_clear_fifo        ; Clear any additional garbage
    jr      _loop                 ; Wait for next key

; Cursor Movement Keys 
_cursor_key:
    cp      158                   
    jr      z,_cursor_left
    cp      142                    
    jr      z,_cursor_right
    cp      155                   
    jr      z,_cursor_home
    cp      154                   
    jr      z,_cursor_end
    jr      _loop

_cursor_left:
    ld      a,b         
    or      a                     ; If at beginning of line
    jr      z,_beep               ;   Beep and loop
    dec     b                     
    dec     hl
    call    _move_left            ; Move cursor left
    jr      _loop

_cursor_right:
    ld      a,b
    sub     c                     ; If CurPos >= LinLen
    jr      nc,_beep              ;   Beep and loop
    inc     b
    inc     hl
    call    _move_right
    jr      _loop

_cursor_home:
    ld      a,b         
    or      a                     ; If at beginning of line
    jr      z,_loop
    dec     b                     
    dec     hl
    call    _move_left            ; Move cursor left
    jr      _cursor_home

_cursor_end:
    ld      a,b
    sub     c                     ; If CurPos >= LinLen
    jr      nc,_loop              ;   Done
    inc     b                     
    inc     hl
    call    _move_right           ; Move cursor left
    jr      _cursor_end


; Control-Key combinations
_ctrlkey
    cp      'M'-64                ; Enter
    jr      z,_return             ; 
    cp      'H'-64                ; Backspace
    jr      z,_back_upspace
    jr      _loop

_back_upspace
; ToDo: move text after cursor to left
    jr      _beep
    ld      a,b 
    or      a                     ; If beginning of line
    jr      z,_beep               ;   Beep and wait for next key
    dec     b                     ; Else
    dec     c
    dec     hl                    ;   Decrement CurPos and CurAdr
    call    _move_left            ;   Move cursor left
    ld      (de),a
    jp      _tloop                ;   and wait for next key

    exx
    ret

_return:
    ld      a,(TTYPOS)
    or      a
    call    z,_line_feed
    ex      de,hl                 ; DE = PosAdr
    pop     hl                    ; HL = BufAdr
    ret
    
_move_left:
    ld      de,(CURRAM)
    ld      a,(TTYPOS)
    or      a
    jr      nz,.notzero           ; If TTYPOS = 0
    dec     de                    ;   Back up to end of previous line
    dec     de
    ld      a,(LINLEN)
    dec     a
.notzero
    dec     a                     ; Back up TTY POS
    ld      (TTYPOS),a            ;   Set TTYPOS to end of line
    dec     de                    
    ld      (CURRAM),de
    jr      _set_curchar          ; Update CURCHR

_move_right:
    push    bc
    ld      a,(LINLEN)
    ld      c,a
    dec     c                     ; C = LastCol
    ld      de,(CURRAM)
    ld      a,(TTYPOS)
    inc     de
    inc     a
    cp      c
    jr      c,.notlast            ; If TtyPos >= LastCol
    inc     de
    inc     de
    inc     a
    inc     a
.notlast
    ld      (TTYPOS),a
    ld      (CURRAM),de
    pop     bc
    jr      _set_curchar          ; Update CURCHR

   
; Print buffer from current position to end of input
; Preserves position in buffer and on screen
_print_buffer:
    ld      a,c
    sub     a,b                   ; A = ChrCnt
    ret     z                     ; If Not 0
    push    bc                    ;   Stack = PosLen, RtnAdr
    push    hl                    ;   Stack = CurAdr, PosLen, RtnAdr
    ld      b,a                   ;   B = ChrCnt
    call    _get_ttypos_curram    ;   A = TtyPos, DE = CurRAM
    push    af                    ;   Stack = TtyPos, CurAdr, PosLen, RtnAdr
.print_loop
    ld      a,(hl)                ;   Print the characters
    rst     OUTCHR                ;   
    inc     hl                    ;   
    djnz    .print_loop
    ld      a,' '
    ld      hl,(TTYPOS)
    ld      (hl),a                ;   Kill the cursor
    pop     af                    ;   A = TtyPos; Stack = CurAdr, PosLen, RtnAdr
    call    _set_ttypos_curram    ;   Restore TTYPOS and CURRAM
    pop     hl                    ;   HL = CurAdr; Stack = PosLen, RtnAdr
    pop     bc                    ;   BC = PosLen; Stack = RtnAdr
    ret

_get_ttypos_curram:
    ld      a,(TTYPOS)
    ld      de,(CURRAM)
    ret

_set_ttypos_curram:
    ld      (TTYPOS),a
    ld      (CURRAM),de
    ret

_line_feed:
    ld      a,10
_out_char:
    rst     OUTCHR             
_res_char:
    ld      de,(CURRAM)           ; Remove BASIC cursor
    ld      a,(CURCHR)
    ld      (de),a
    ret

_set_curchar:
    ld      de,(CURRAM)           
    ld      a,(de)
    ld      (CURCHR),a
    ret

; Move cursor left

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
    ld      hl,LINLEN
    ld      a,c                   ; A = MaxLen
    cp      37            
    ccf                           ; If MaxLen > 36
    ret     c                     ;   Return Carry Set
    add     a,d                   ; A = Column + MaxLen
    cp      (hl)
    ccf                           ; If end of field past end of line
    ret     c                     ;   Return Carry Set
    ld      a,e                   ; A = Row
    cp      25
    ccf                           ; If Row > 25
    ret     c                     ;   Return Carry Set
    call    move_cursor           ; Move to input point
    ld      de,BUF
    ld      l,0                   ; B = CurLen
_input_loop:
    call    key_read_ascii        ; Get keypress
    jr      z,_input_loop         ; If no key, try again
    cp      3                     ; If Ctrl-C
    jr      z,_abort_input        ;   Abort
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
    xor     a
    ld      (de),a                ; Terminate entry
    ld      a,l                   ; A = CurLen
    cp      b                     ; If CurLen >= MinLen
    ld      hl,BUF                ;   HL = BufAdr
    ret     nc                    ;   Return Carry Clear
_abort_input:
    or      $FF                   ; Return Carry Clear, Sign Set
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
    push    hl                    ; Stack = CurLen, RtnAdr
    ld      hl,(CURRAM)           
    ld      (hl),a                ; Write character to screen
    inc     hl
    ld      (CURRAM),hl
    ld      a,(hl)
    ld      (CURCHR),a
    pop     hl
    jr      _input_loop           ; and loop

