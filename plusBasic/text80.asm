;=====================================================================================
; 80 Column Text Screen Routines
;======================================================================================

;-----------------------------------------------------------------------------
; Clear 80 column text screen to default colors and initialize cursor
; Clobbers: A,BC,DE
;-----------------------------------------------------------------------------
    ld      a,6                   ; default to black on cyan
;-----------------------------------------------------------------------------
; Clear 80 column text screen and initialize cursor
; Input: A: Foreground/background color byte
; Clobbers: A,BC,DE
;-----------------------------------------------------------------------------
cls80:
    push    hl
    call    clear_screen
    pop     hl
_init_cursor
    ld      a,' '
    ld      (CURCHR),a            ; SPACE under cursor
    ld      de,SCREEN+81
    ld      (CURRAM),de
    xor     a
    ld      (TTYPOS),a            ; column 0
    ret

;-----------------------------------------------------------------------------
; Clear 80 column text screen
; Input: A: Foreground/background color byte
; Clobbers: A,BC,DE,HL
;-----------------------------------------------------------------------------
clear_screen80:
    set     7,b
    push    bc
    call    .fill80               ; Fill COLOR RAM
    pop     bc
    res     7,b
    ld      a,' '
.fill80
    out     (c),b
    ld      hl,SCREEN
    ld      bc,2048
    jp      sys_fill_mem

;-----------------------------------------------------------------------------
; Print character to 80 column screen with pause every 23 lines
; Input: A: Character to print
;-----------------------------------------------------------------------------
ttychr80pop:
    pop     af
ttychr80:
    push    af                    ; Save character
    cp      10                    ; If LineFeed
    jr      z,_islf
    ld      a,(TTYPOS)
    or      a                     ;   At beginning of line?
    jr      nz,_islf              ;   No, skip line counter check
    ld      a,(CNTOFL)  
    or      a                     ;   Is line Counter 0?
    jr      z,_islf               ;   Then no pauses
    dec     a 
    ld      (CNTOFL),a            ;   Decrement Line Counter
    jr      nz,_islf              ;   Not 0, don't pause
    ld      a,23  
    ld      (CNTOFL),a            ;   Reset line counter
    call    TRYIN                 ;   Wait for character from keyboard
_islf:
    pop     af                    ; Restore Character
;-----------------------------------------------------------------------------
; Print character to 80 column screen
; Input: A: Character to print
;-----------------------------------------------------------------------------
ttyout80:
    push    af
    exx                           ;
    cp      7                     ; Is it BEL!
    jp      z,BEEP                ; Make Beep Sound
    cp      11                    ; Is it CLS?
    jp      z,_ttyclr             ; Clear the Screen
    ld      e,a                   ; Save A
    ld      hl,(CURRAM)
    ld      a,(CURCHR)            ; Get character under cursor
    ld      (hl),a                ; Place at current screen position
    ld      a,e                   ; Restore A
    cp      8                     ; Is it BS?
    jr      z,_bs                 ; Do Back Space
    cp      13                    ; Is it CR?
    jr      z,_cr                 ; Do Carriage Return
    cp      10                    ; Is it LF?
    jr      z,_lf                 ; Do Line Feed
    ld      hl,(CURRAM)           ; Place character at
    ld      (hl),a                ; current position on screen
    call    ttymove80             ; Move Cursor Right
    jr      _tty_finish           ; Finish Up

; Backspace: Move Cursor Left and Delete Character
_bs:
    ld      a,(TTYPOS)
    or      a                     ; If not at position 0
    jr      nz,.do_bs             ; Do the backspace
    ld      c,a
    ld      a,h
    cp      $30
    jr      nz,.not_home
    ld      a,l
    cp      81                    ; If at home position
    ld      a,c
    jp      z,.no_bs              ;   Don't backspace
.not_home
    dec      hl                   ; Backup to end of previous line
    dec      hl
    ld       a,78
.do_bs                                                                              
    dec     hl                    ; No, Move One to the Left
    dec     a
.no_bs
    ld      (hl),' '              ; Erase Character at Position
; Save Character and Display Cursor
_tty_savef:
    call    TTYSAV                ; Save Column and Position
_tty_finish:
    ld      hl,(CURRAM)
    ld      a,(hl)                ; Get character at position
    ld      (CURCHR),a            ; Save character under cursor
    ld      (hl),$7F              ; Display Cursor
_exx_pop_ret:
    exx
    pop     af
    ret

; Carriage Return: Move Cursor to Beginning of Current Line
_cr:
    ld      de,(TTYPOS)
    xor     a                     ;
    ld      d,a                   ; Subtract Cursor Column
    sbc     hl,de                 ; from Screen Position
    jr      _tty_savef

; Line Feed: Move Cursor Down One Line
_lf:
    ld      de,SCREEN+1920
    rst     COMPAR                ; Cursor on Last Row?
    jp      nc,.lf_scroll         ; Yes, Scroll and Finish Up
    ld      de,80
    add     hl,de                 ; Add 80 to Move Down One Line
    ld      (CURRAM),hl           ; Save New Screen Position
    jr      _tty_finish
.lf_scroll
    call    scroll80              ; Scroll Up and Keep Screen Position
    jr      _tty_finish

; Scroll Screen Up one Line
scroll80:
    push    af
    ld      a,(SCREENCTL)         ; 
    rla                           ; Carry = SCRCOLOR
    jr      nc,.nocolor           ; If Color PRINT enabled
    in      a,(IO_VCTRL)
    set     7,a                   ;   Select COLOR RAM
    out     (IO_VCTRL),a          ;   Select Screen RAM
    push    af
    ld      a,(SCOLOR)            ;   Get screen color
    call    .scroll
    pop     af
    res     7,a
    out     (IO_VCTRL),a          ;   Select Screen RAM
.nocolor
    ld      a,' '
    call    .scroll
    pop     af
    ret   
.scroll    
    ld      bc,1920               ; Move 23 * 80 bytes
    ld      de,SCREEN+80          ; To Row 1 Column 0
    ld      hl,SCREEN+160         ; From Row 2 Column 1
    ldir                          ;
    ld      b,80                  ; Loop 80 Times
    ld      hl,SCREEN+1921        ; Starting at Row 23, Column 0
.loop
    ld      (hl),a                ; Put Space
    inc     hl                    ; Next Column
    djnz    .loop                 ; Do it again
    ret

_ttyclr:
    call    set_color_off         ; Turn off color printing
    ld      a,6                   ;   A = Default Colors
    call    cls80                 ; Clear Screen
    ld      de,$7F                ; Display Cursor
    jr      _exx_pop_ret

ttymove80:
    ld      a,(SCREENCTL)         ; 
    rla                           ; Carry = SCRCOLOR
    jr      nc,.nocolor           ; If Color PRINT enabled
    in      a,(IO_VCTRL)
    set     7,a                   ; Select COLOR RAM
    out     (IO_VCTRL),a          ; Select Screen RAM
    push    af
    ld      a,(SCOLOR)            ; Get screen color
    ld      (hl),a                ; Write to color RAM
    pop     af
    res     7,a
    out     (IO_VCTRL),a          ; Select Screen RAM
.nocolor
    ld      a,(TTYPOS)
    inc     hl                    ;Increment Position in Memory
    inc     a                     ;Increment Cursor Column
    cp      78                    ;Less than 79?
    jp      c,TTYSAV              ;Save and Return
    inc     hl
    inc     hl                    ;Skip border columns
    ld      de,SCREEN+2000
    rst     COMPAR                ;Past End of Screen?
    ld      a,0                   ;Cursor Column = 0
    jp      c,TTYSAV              ;No, Save Position and Column
    ld      hl,SCREEN+1921        ;Yes, Position = Row 24, Column 0
    call    TTYSAV                ;Save Position and Column
    jp      scroll80              ;Scroll Screen



set_linlen:
    in      a,(IO_VCTRL)
set_linlen_a:
    and     VCRTL_80COL_EN
    ld      a,40                  ; A = 40
    jr      z,.not80              ; If 80 column bitset
    rla                           ;   A = 80
.not80
    ld      (LINLEN),a            ; Save it
    ret

;-----------------------------------------------------------------------------
; Hook 13 - OUTDO 
;-----------------------------------------------------------------------------
outdo_hook:
  push    af                
  ld      a,(BASYSCTL)            ; Get System Control Bits
  rra                             ; Carry = BASOUTBUF
  jp      c, output_to_buffer     ; If bit set, write to buffer 
  pop     af
  jp      OUTCON

;-----------------------------------------------------------------------------
; Hook 19 - TTYCHR (Print Character to Screen)
;-----------------------------------------------------------------------------
ttychr_hook:
    push    af
    cp      11
    jr      nz,.not_cls
    ld      a,(BASYSCTL)
    and     ~BASOUTBUF
    ld      (BASYSCTL),a
.not_cls
    in      a,(IO_VCTRL)
    and     VCRTL_80COL_EN
    jp      nz,ttychr80pop
    pop     af
    push    af                ;;Save character
    cp      10                ;[M80] LINE FEED?
    jp      TTYILF
