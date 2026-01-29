;=====================================================================================
; 80 Column Text Screen Routines
;======================================================================================

; Extended control keys in color printing mode

tty_pos:
    ld      a,(TTYPOS)
    ld      de,(LINLEN)           ; E = LinLen
    ld      d,e
    dec     d
    dec     d                     ; D = LinWid
    ret

lf_hook:
    ld      a,(LINLEN)
    cp      40
    ld      de,SCREEN+960
    ret     z
    ld      de,SCREEN+1920
    ret
    
ttyclr_hook:
    push    bc
    ld      iy,set_color_off
    call    gfx_call
    ld      iy,reset_bordermap
    call    gfx_call
    call    set_cursor_on
    call    set_scroll_on
    pop     bc
    ld      hl,SCREEN
    ld      a,(LINLEN)
    cp      40
    jp      z,TTYCLF
    ld      iy,screen_clear
    call    gfx_call
    ld      hl,SCREEN+81
    jp      TTYCLX

;;; ToDo: Implement this at a later time
;;; Add a command to enable/disable extended control codes
ttyout_hook:
    jp      z,BEEP
    jp      TTYCLK
    ld      e,a
    ld      a,(SCREENCTL)
    rra                           ; Set carry if extended control chars
    ld      a,e                   ; Restore character
    jr      nc,.ttyclk
    cp      1
    jr      z,_soh
    cp      2
;   jr      z,_stx
    cp      12
    jr      z,_ff
.ttyclk
    ld      a,e
    jp      TTYCLK

; Ctrl-L - Clear screen to current colors
_ff:
    ld      iy,screen_clear
    call    aux_call
; Ctrl-A - Move cursor to top left of screen
_soh:
    call    home_cursor
    jp      TTYXPR
; Ctrl-B - Non-destructive backspace
;;; THIS IS WREAKING HAVOC!
_stx:
    call    BKSPC
    jp      TTYFIN

cursor_put:
    ld      hl,(CURRAM)           ;
    ld      a,(hl)
    ld      (CURCHR),a            ; Save character under cursor
    ld      a,(SCREENCTL)
    and     CRSR_OFF
    ret     nz                    ; If CURSOR ON
    ld      a,$7F                 ; A = Cursor
; Write character in A to current screen location
; If Color Printing enabled, write current colors to location in Color RAM
; Clobbers: A, BC, HL
tty_put:
    ld      hl,(CURRAM)           ; HL = ScrnAdr
    ld      (hl),a                ; Write the character
    ld      a,(SCREENCTL)         ; 
    rla                           ; Carry = SCRCOLOR
    ret     nc                    ; If Color Printing Enabled
    ld      a,(SCOLOR)            ;   Get screen color
color_put:
    ld      c,IO_VCTRL
    in      b,(c)                 ;   B = VCTRL
    bit     6,b
    jr      nz,.col80             ;   If 40 Column Mode
    set     2,h                   ;     HL = ColrAdr
    ld      (hl),a                ;     Write color
    ret                           ;   Else
.col80
    set     7,b
    out     (c),b                 ; Switch to color page
    ld      (hl),a
    res     7,b                   ; Switch to text page
    out     (c),b                 ; Switch to color page
    ret

tty_linlen:
    ld      de,(LINLEN)
    ld      d,0
    ret

ttymov_hook:
    ld      a,(LINLEN)
    cp      40
    ld      de,SCREEN+1000        ; End of Screen
    ld      bc,SCREEN+961         ; Start of Last Row
    ret     z
    ld      de,SCREEN+2000        ; End of Screen
    ld      bc,SCREEN+1921        ; Start of Last Row
    ret
    
scroll_hook:
    ld      a,(SCREENCTL)
    and     SCROLLOFF
    ret     nz
    ld      a,(LINLEN)
    cp      40                    ; Set NZ if 80 columns
    ld      a,(SCREENCTL)         ; 
    rla                           ; Set Carry if Color PRINT
    ld      a,(SCOLOR)            ; A = Screen Colors
    jr      nz,.col80             ; If 40 columns
    ld      bc,920                ;   If not Color Print
    jp      nc,SCROLC             ;     Do standard scroll
    ld      de,COLOR+40
    ld      hl,COLOR+80
    call    .scroll40
    ld      a,' '
    ld      de,SCREEN+40
    ld      hl,SCREEN+80
.scroll40                           
    ld      bc,920
    ldir    
    ld      b,40
    jp      .loop
.col80    
    jr      nc,.screen80          ; If Color PRINT enabled
    ld      c,IO_VCTRL
    in      b,(c)
    push    bc                    ;   Stack = VCTRL, RtnAdr
    set     7,b                   ;   Switch to color page
    out     (c),b                 ;   
    call    .scroll80             ;   Scroll Color RAM
    pop     bc                    ;   BC = VCTRL; Stack = RtnAdr
    res     7,b                   ;   Switch to screen page
    out     (c),b                 ;   
.screen80
    ld      a,' '                 ; Scroll Screen RAM   
.scroll80
    ld      bc,1840
    ld      de,SCREEN+80
    ld      hl,SCREEN+160
    ldir
    ld      b,80
.loop 
    ld      (de),a                ; Put Space
    inc     de                    ; Next Column
    djnz    .loop                 ; Do it again
    ret

set_scroll_on:
    ld      a,$FF
set_scroll:
    xor     $FF                   ; Invert so 0 disables
    and     SCROLLOFF
    ld      c,a
    ld      a,(SCREENCTL)
    and     $FF-SCROLLOFF
    or      c
    ld      (SCREENCTL),a
    ret

set_linlen:
    in      a,(IO_VCTRL)
set_linlen_a:
    and     VCRTL_80COL_EN
    ld      a,40                  ; LinLen = 40
    ld      b,0                   ; GfxMode = 40 column
    jr      z,.not80              ; If 80 column bitset
    rla                           ;   A = 80
    inc     b                     ;   GfxMode = 80 column
.not80
    ld      (LINLEN),a            ; Save it
    ld      a,(GFX_FLAGS)
    and     $FF-GFXM_WIDE
    or      b
    ld      (GFX_FLAGS),a
    ret
