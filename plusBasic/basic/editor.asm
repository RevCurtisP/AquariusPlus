;=====================================================================================
; Enhanced direct mode Line Editor
;=====================================================================================

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
    call    _toggle_cursor        ; Display cursor
.wait
    call    key_read_ascii        ; Wait for Key
    jr      z,.wait
    call    _toggle_cursor        ; Display cursor
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
    jr      z,.backspace
    jr      _loop

.backspace
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
.loop
    ld      a,(hl)                ;   Print the characters
    rst     OUTCHR                ;   
    inc     hl                    ;   
    djnz    .loop                 ;   
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

; Invert Screen colors at cursor position
; Clobbers: DE
_toggle_cursor:
    push    af
    ld      de,(CURRAM)           ; DE = ScrnAdr
    in      a,(IO_VCTRL)
    bit     6,a
    jr      z,.toggle40           ; If 80 columns
    set     7,a                   
    out     (IO_VCTRL),a          ;   Select Color RAM page
    ex      af,af'
    call    .toggle               ;   Swap nybbles
    ex      af,af'
    res     7,a                   ;   
    out     (IO_VCTRL),a          ;   Select screen RAM age
    pop     af
    ret
.toggle40                         ; Else
    set     2,d                   ;   DE = ColrAdr
    call    .toggle               ;   Swap nybbles
    pop     af
    ret

.toggle
    ld      a,(de)                ; 
    or      a                     ; 0 abcd efgh
    rla                           ; a bcde fgh0
    adc     0                     ; 0 bcde fgha
    rla                           ; b cdef gha0
    adc     0                     ; 0 cdef ghab
    rla                           ; c defg hab0
    adc     0                     ; 0 defg habc
    rla                           ; d efgh abc0
    adc     0                     ; 0 efgh abcd
    ld      (de),a                  
    ret
    