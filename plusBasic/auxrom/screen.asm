;~===========================================================================~
; Text Screen Auxiliary ROM Routines
;
; LD IY,_routine_name_
; CALL AUX_CALL
;~===========================================================================~

; Input: D: Column, E: Row
; Output: HL: Screen RAM address
move_cursor:
    push    af
    ; Restore character behind cursor
    push    hl
    exx
    ld      a,(SCREENCTL)
    and     CRSR_OFF
    jr      nz,.no_cursor
    ld      hl,(CURRAM)         ; CHRPOS - address of cursor within matrix
    ld      a,(CURCHR)          ; BUFO - storage of the character behind the cursor
    ld      (hl), a             ; Put original character on screen
.no_cursor
    pop     hl
    call    cursor_offset
    ld      de, SCREEN          ; Screen character-matrix (= 12288 dec)
    add     hl, de              ; Putting it all together
    ex      af,af'
    jp      TTYFIS              ; Save cursor position and return


; Input: H: Column, L: Row
; Output: HL: Screen offset
cursor_offset:
    ; Get Video Mode
    ld      a,(LINLEN)
    cp      80                  ; Set Zero Flag if 80 columns
    ex      af,af'
    ; Calculate new cursor location
    ld      a, l
    add     a, a
    add     a, a
    add     a, l
    ex      de, hl
    ld      e, d
    ld      d, $00
    ld      h, d
    ld      l, a
    ld      a, e
    dec     a
    add     hl, hl
    add     hl, hl
    add     hl, hl              ; HL is now 40 * rows
    ex      af,af'
    jr      nz,.not80
    add     hl, hl              ; HL is now 80 * rows
.not80
    add     hl, de              ; Added the columns
    ret

screen_clear_color_a:
    ld      c,a
    ld      a,(LINLEN)
    cp      40                    ; NZ = 80 columns
    ld      a,c
    jr      _clear_color
;-----------------------------------------------------------------------------
; Fill Color RAM with current/default colors
;-----------------------------------------------------------------------------
screen_clear_color:
    call    _screen_width_colors
_clear_color:
    jr      nz,_color_clear80
    jr      _color_clear40

;-----------------------------------------------------------------------------
; Clear Text Screen
; Input: A: Color
; Clobbered: A, BC, DE, HL
;-----------------------------------------------------------------------------
screen_clear_a:
    ld      c,a
    ld      a,(LINLEN)
    cp      40                    ; NZ = 80 columns
    ld      a,c
    jr      _clear_screen
;-----------------------------------------------------------------------------
; Clear Text Screen
; Clobbered: A, BC, DE, HL
;-----------------------------------------------------------------------------
screen_clear:
    call    _screen_width_colors
_clear_screen:
    jr      nz,_screen_clear80
    call    _color_clear40
    jr      _text_clear40
_color_clear40:
    ld      hl,COLOR
    jr      _fill40
_text_clear40:
    ld      a,' '
    ld      hl,SCREEN
_fill40:
    ld      bc,1000
    jp      sys_fill_mem
    
_screen_clear80:
    call    _color_clear80
    jr      _text_clear80
_color_clear80:
    ld      c,IO_VCTRL
    in      b,(c)
    set     7,b
    out     (c),b
    push    bc
    call    _fill80               ; Fill Color RAM
    pop     bc
    res     7,b
    out     (c),b
    ret
_text_clear80:
    ld      a,' '
_fill80:
    ld      hl,SCREEN
    ld      bc,2000
    jp      sys_fill_mem


;-----------------------------------------------------------------------------
; Copy Current Screen to Paged RAM
; Input: A: Page
;        B: 1 = Character Matrix, 2 = Color Matrix, 3 = Both
;       DE: Address
; Clobbered: A,AF',BC,DE,HL
;-----------------------------------------------------------------------------
;;; ToDo: Move to new file gfxrom/screencopy.asm
; COPY SCREEN TO @32,0
; COPY SCREEN TO @34,0 FAST
; SCREEN 3:COPY SCREEN TO @33,0:SCREEN 1
; COPY @32,0 TO SCREEN
; COPY @34,0 TO SCREEN FAST
; SCREEN 3:COPY @33,0 TO SCREEN:SCREEN 1
screen_read_fast:
    ld      iy,_paged_to_screen_fast
    jr      _screen_read
screen_read_paged:
    ld      iy,_paged_to_screen
_screen_read:
    ex      de,hl                 ; HL = Address
    jr      _read_write_screen
screen_write_fast:
    ld      iy,_screen_to_paged_fast
    jr      _read_write_screen
screen_write_paged:
    ld      iy,_screen_to_paged
_read_write_screen:
    push    bc                    ; Stack = ChrClr, RtnAdr
    ld      c,IO_VCTRL
    in      b,(c)          
    bit     6,b                   ; Set Z if 40-column mode
    pop     bc                    ; B = ChrClr, RtnAdr
    jp      z,jump_iy             ; If 40-columns, do it an return
    call    screen_textpage_0
    push    bc                    ; Stack = ChrClr, RtnAdr
    push    af                    ; Stack = Page, ChrClr, RtnAdr
    bit     0,b
    ld      b,3                   ; Copy 2048 bytes
    call    nz,jump_iy            ; Write Screen RAM
    pop     af                    ; A = Page; Stack = ; Stack = ChrClr, RtnAdr, RtnAdr
    pop     bc                    ; B = ChrClr, RtnAdr
    call    screen_textpage_1
    bit     1,b
    ld      b,3                   ; Copy 2048 bytes
    call    nz,jump_iy            ; Write Color RAM
    jr      screen_textpage_0
_screen_to_paged_fast:
    ld      ix,page_fast_write_bytes
    jr      _to_paged
_screen_to_paged:
    ld      ix,page_write_bytes
_to_paged:
    ld      hl,SCREEN
    bit     0,b
    jr      nz,_copy
    ld      hl,COLOR
    jr      _copy
_paged_to_screen_fast:
    ld      ix,page_fast_read_bytes
    jr      _to_screen
_paged_to_screen:
    ld      ix,page_read_bytes
_to_screen:
    ld      de,SCREEN
    bit     0,b
    jr      nz,_copy
    ld      de,COLOR
_copy:
    ex      af,af'                ; A' = Page
    ld      a,b                   ; A = ChrClr
    ld      bc,$0800              ; Copy 2048 bytes
    cp      3                    
    jr      z,.copy               ; If not chars and colors
    ld      b,$04                 ;   Copy 1024 bytes
.copy
    ex      af,af'                ; A' = Page
    push    ix
    call    jump_ix
    pop     ix
    ret

screen_textpage_1:
    push    af
    in      a,(IO_VCTRL)
    or      a,VCTRL_TEXT_PAGE
    jr      _set_text_page
screen_textpage_0:
    push    af
    in      a,(IO_VCTRL)
    and     a,$FF-VCTRL_TEXT_PAGE
_set_text_page:
    out     (IO_VCTRL),a
    pop     af
    ret

_check_offset_len:
    ld      a,d
    rla                           ; If DE is negative
    ret     c                     ;   Return carry set
    call    _exdehl_max_ofs       ; DE = MaxOfs, HL = ScrOfs
    ex      af,af'                ; F' = WidFlg
    dec     hl                    
    add     hl,bc                 ; HL = Strend
    rst     COMPAR                ; 
    ccf                           ; If StrEnd >= MaxOfs      
    ret     c                     ;   Return Carry Set
    ld      a,b
    or      c                     ; Set Z if null string                   
    ret
    

_bdehl_max_ofs:
    ld      b,a   
;-----------------------------------------------------------------------------
; Calculate Maximum Screen Offset
; Input: DE: Address
; Output: A: VCRTL_80COL_EN bit of IO_VCTRL
;        DE: Max Offset
;        HL: Address
; Flags: Z if 40 columns, NZ if 80 columns
;-----------------------------------------------------------------------------
_exdehl_max_ofs:
    ex      de,hl                 ; HL = Address
;-----------------------------------------------------------------------------
; Calculate Maximum Screen Offset
; Input: A: Byte
;       DE: Address
; Output: A: VCRTL_80COL_EN bit of IO_VCTRL
;        DE: Max Offset
; Flags: Z if 40 columns, NZ if 80 columns
;-----------------------------------------------------------------------------
;screen_max_ofs:
    ld      de,1024               ; MaxOfs = 1024
    in      a,(IO_VCTRL)
    and     VCRTL_80COL_EN
    ret     z                     ; If 80 columns
    rl      d                     ;   MaxOfs = 2048
    or      a                     ; Set NZ flag
    ret

; Output: A = Colors, BC = ScrWid, NZ = 80 columns
_screen_width_colors:
    ld      a,(LINLEN)
    cp      40                    ; NZ = 80 columns
    ld      c,a
    ld      b,0
_screen_colors:
    ld      a,(SCREENCTL)         ; 
    rla                           ; Carry = SCRCOLOR
    ld      a,DFLTATTRS
    ret     nc
    ld      a,(SCOLOR)
    ret
