;-----------------------------------------------------------------------------
; Initialize screen buffers
; Called from _coldboot
;-----------------------------------------------------------------------------
init_screen_buffers:
    ld      a,SCR_BUFFR           ; Swap Screen Buffers into Page 1
    out     (IO_BANK1),a          ; Stack is hidden

    ld      a,' '
    ld      hl,BANK1_BASE+SCRN40BUF
    ld      bc,1024
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN41BUF
    ld      bc,1024
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN80BUF
    ld      bc,2048
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN40SWP
    ld      bc,1024
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN41SWP
    ld      bc,1024
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN80SWP
    ld      bc,2048
    call    sys_fill_mem

    ld      a,6 
    ld      hl,BANK1_BASE+SCRN40BUF+1024
    ld      bc,1024
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN41BUF+1024
    ld      bc,1024
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN80BUF+2048
    ld      bc,2048
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN40SWP+1024
    ld      bc,1024
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN41SWP+1024
    ld      bc,1024
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN80SWP+2048
    ld      bc,2048
    call    sys_fill_mem

    ld      a,RAM_BAS_1
    out     (IO_BANK1),a
    ret

;-----------------------------------------------------------------------------
; initialization screen variable buffers
; Exits with Extended ROM in Bank 3
; Output: B: (TTYPOS)
;         C: (CURCHR)
;        DE: (CURRAM)
;        HL: Address after end of last Save Buffer
;-----------------------------------------------------------------------------
init_screen_vars:
    ld      bc,' '                  
    ld      de,SCREEN+41
    ld      hl,BUFSCRN40
    call    write_screen_vars     ; 40 column primary
    call    write_screen_vars     ; 40 column secondary
    ld      de,SCREEN+81
    call    write_screen_vars     ; 80 column
    ld      de,SCREEN+41
    call    write_screen_vars     ; 40 column primary
    call    write_screen_vars     ; 40 column secondary
    ld      de,SCREEN+81
    jp      write_screen_vars     ; 80 column

_screen_vars_offset
    in      a,(IO_VCTRL)
    ld      l,BUFSCRN80
    bit     6,a
    ret     nz
    ld      l,BUFSCRN41
    bit     7,a
    ret     nz
    ld      l,BUFSCRN40
    ret

get_screen_vars:
    ld      a,(TTYPOS)
    ld      b,a
    ld      a,(CURCHR)
    ld      c,a
    ld      de,(CURRAM)
    ret

;-----------------------------------------------------------------------------
; Reset Screen to Text only and default palette
; Clobbers: A,BC,D,HL
;-----------------------------------------------------------------------------
screen_reset:
    ld      a,VCTRL_TEXT_EN
    out     (IO_VCTRL),a
    xor     a
    jp      palette_reset

;-----------------------------------------------------------------------------
; Switch Text Screen
; Input: A: Screen # - 1,2 = 40 column, 3 = 80 column
;-----------------------------------------------------------------------------
screen_switch:
    push    af
    ld      a,BUFSCRN40           ; Save current text screen to buffer
    ld      hl,SCRN40BUF
    call    screen_save           
    pop     af

    ld      b,40                  ; Default screen width to 40
    cp      3                     
    jr      c,.not80              ; If 80 column mode
    rl      b                     ;   Make it 80
.not80
    ld      hl,LINLEN
    ld      (hl),b                ; Write out screen width
    ld      de,.text_mode_table
    call    table_lookup          ; Look up Text Mode bits
    ld      b,a                   ; B = Mode Bits
    in      a,(IO_VCTRL)
    and     $3E                   ; Text Mode mask
    or      b                     ; A = New VCTRL
    out     (IO_VCTRL),a

    ld      a,BUFSCRN40           ; Restore current text screen from buffer
    ld      hl,SCRN40BUF
    call    screen_restore       

    ret

.text_mode_table:
    byte    VCTRL_TEXT_OFF                  ; 0 = Text Off
    byte    VCTRL_TEXT_EN                   ; 1 = 40 Column Primary
    byte    VCTRL_TEXT_EN+VCTRL_TEXT_PAGE   ; 2 = 40 Column Secondary
    byte    VCTRL_TEXT_EN+VCRTL_80COL_EN    ; 3 = 80 Column



;-----------------------------------------------------------------------------
; Save screen vars for current screen
; Clobbers: AF, BC, DE, HL
;-----------------------------------------------------------------------------
swap_screen_vars:
    ld      a,SWPSCRN40
    call    get_screen_vars
    push    de                    ; Stack = CURRAM, RtnAdr
    push    bc                    ; Stack = TTYPOS, CURCHR, CURRAM, RtnAdr
    ld      a,SWPSCRN40
    call    restore_screen_vars   ; 
    pop     bc                    ; B = TTYPOS, C = CURCHR; Stack = CURRAM, RtnAdr
    pop     de                    ; DE = CURRAM; Stack = RtnAdr
    ld      a,SWPSCRN40
    push    af
    jr      _save_screen_vars

;-----------------------------------------------------------------------------
; Save screen vars for current screen
; Input: A: BUFSCRN40 or SWPSCRN40
; Clobbers: AF, BC, DE, HL
;-----------------------------------------------------------------------------
restore_screen_vars:
    push    af
    call    _screen_vars_offset
    pop     af
    add     a,l
    ld      l,a
    call    read_screen_vars
    ld      a,b
    ld      (TTYPOS),a
    ld      a,c
    ld      (CURCHR),a
    ld      (CURRAM),de
    ret

;-----------------------------------------------------------------------------
; Save screen vars for current screen
; Input: A: BUFSCRN40 or SWPSCRN40
; Clobbers: AF, BC, DE, HL
;-----------------------------------------------------------------------------
save_screen_vars:
    push    af
    call    get_screen_vars
_save_screen_vars
    call    _screen_vars_offset
    pop     af
    add     a,l
    ld      l,a

;-----------------------------------------------------------------------------
; Write to screen variable buffers
; Input: B: TTYPOS
;        C: CURCHR
;       DE: CURRAM
;       HL: Save Buffer offset
;-----------------------------------------------------------------------------
; Output: HL: Address after end of Save Buffer
; Clobbers: AF
write_screen_vars:
    ld      a,BAS_BUFFR
    out     (IO_BANK1),a
    ld      h,$40                 ; HL = Bank 3 Address
    ld      (hl),b                ; SAVTTYPOS
    inc     hl
    ld      (hl),c                ; SAVCURCHR
    inc     hl
    ld      (hl),e                ; SAVCURRAM
    inc     hl
    ld      (hl),d                
    inc     hl
    ld      a,RAM_BAS_1
    out     (IO_BANK1),a
    ret

;-----------------------------------------------------------------------------
; Read from screen variable buffers
; Exits with Extended ROM in Bank 3
; Input: HL: Save Buffer offset
; Output: B: TTYPOS
;         C: CURCHR
;        DE: CURRAM
;        HL: Address after end of Save Buffer
;-----------------------------------------------------------------------------
read_screen_vars:
    push    af
    ld      a,BAS_BUFFR
    out     (IO_BANK1),a
    ld      h,$40                 ; HL = Bank 3 Address
    ld      b,(hl)                ; SAVTTYPOS
    inc     hl
    ld      c,(hl)                ; SAVCURCHR
    inc     hl
    ld      e,(hl)                ; SAVCURRAM
    inc     hl
    ld      d,(hl)                
    inc     hl
    ld      a,RAM_BAS_1
    out     (IO_BANK1),a
    pop     af
    ret

;-----------------------------------------------------------------------------
; Copy Screen Buffer to Text Screen
; Input: A: BUFSCRN40 or SWPSCRN40
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_restore:
    push    hl                    ; Stack = ScrBuf, RtnAdr
    call    restore_screen_vars
    pop     hl                    ; HL = ScrBuf, RtnAdr
    ld      ix,.page_fast_read
    jr      _screen
    
.page_fast_read
    ex      de,hl                 ; Swap for read
    jp      page_fast_read_bytes  

;-----------------------------------------------------------------------------
; Copy Text Screen to Screen Buffer
; Input: A: Cursor buffer offset: BUFSCRN40 or SWPSCRN40
;       HL: Screen buffer offset: SCRN40BUF or SCRN40SWP 
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_save:
    push    hl                    ; Stack = ScrBuf, RtnAdr
    call    save_screen_vars
    pop     hl                    ; HL = ScrBuf, RtnAdr
    ld      ix,page_fast_write_bytes
    jr      _screen

;-----------------------------------------------------------------------------
; Swap Text Screen with Screen Buffer
; Input: A: Cursor buffer offset: BUFSCRN40 or SWPSCRN40
;       HL: Screen buffer offset: SCRN40BUF or SCRN40SWP 
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_swap:
    push    hl                    ; Stack = ScrBuf, RtnAdr
    call    swap_screen_vars
    pop     hl                    ; HL = ScrBuf, RtnAdr
    ld      ix,page_mem_swap_bytes   

_screen:
    in      a,(IO_VCTRL)
    bit     6,a                   
    jr      nz,_screen80         ; If 40 column screen
    bit     7,a                   ;   If primary page
    ld      de,SCRN40BUF          ;     Write to primary buffer
    jr      z,_screen40          ;   Else 
    ld      de,SCRN41BUF          ;     Write to auxiliary buffer
    jr      _screen40            ; Else
_screen80
    push    hl                    ;   Stack = BufOfs, RtnAdr
    push    af                    ;   Stack = VCTRL, BufOfs, RtnAdr
    set     7,a                   ;   Write from Color RAM
    out     (IO_VCTRL),a          
    ld      de,SCRN80BUF+2048     ;   to second half of 80-column buffer
    call    _screen40             ;   
    pop     af                    ;   A = VCTRL, Stack = BufOfs, RtnAdr
    pop     hl                    ;   HL = BufOfs, Stack = RtnAdr
    res     7,a                   ;   Write from Screen RAM
    out     (IO_VCTRL),a          
    ld      de,SCRN80BUF          ;   to firat` half of 80-column buffer
_screen40
    add     hl,de                 
    ex      de,hl
    ld      a,SCR_BUFFR
    ld      bc,2048
    ld      hl,SCREEN             ; Copying from Text and Color RAM
    jp      (ix)                  ; Do it

;-----------------------------------------------------------------------------
; Get the screen buffer address offset
; Output: DE: Screen Buffer Offset
; Sets Flags: S if 80 column screen, C if secondary 40 column screen
;-----------------------------------------------------------------------------
screen_get_buff_ofs:
    in      a,(IO_VCTRL)
    rla                           ; Shift TEXT_PAGE into carry, 80COL_EN into bit 7
    ld      de,SCRN80BUF
    ret     m
    ld      de,SCRN41BUF          ;     Write to auxiliary buffer
    ret     c
    ld      de,SCRN40BUF          ;     Write to primary buffer
    ret

;-----------------------------------------------------------------------------
; Update VCTRL Register
; Input: C: Bit Pattern
;        B: Bit Mask 
; Returns: B: New VCTRL value
;-----------------------------------------------------------------------------
screen_set_vctrl:
    push    a
    in      a,(IO_VCTRL)
    and     b
    or      c
    out     (IO_VCTRL),a
    ld      b,a
    pop     a
    ret


