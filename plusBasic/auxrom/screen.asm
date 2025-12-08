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
    ld      hl, (CURRAM)        ; CHRPOS - address of cursor within matrix
    ld      a, (CURCHR)         ; BUFO - storage of the character behind the cursor
    ld      (hl), a             ; Put original character on screen
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

set_color:
    ld      (SCOLOR),a
    ld      a,(SCREENCTL)
    or      SCRCOLOR
    jr      _write_screenctl

set_color_off:
    ld      a,(SCREENCTL)
    push    af
    and     $FF-SCRCOLOR
    call    _write_screenctl
    pop     af      
    and     CRSR_OFF
    ret     nz
;    push    hl
;    ld      hl,(CURRAM)
;    ld      a,DFLTATTRS
;    call    color_put
;    pop     hl
    ret

_write_screenctl:
    ld      (SCREENCTL),a
    and     SCRCOLOR              ; Isolate Screen Control bit
    ld      c,a                   ; C = ScrColor
.direct
    call    in_direct
    ret     c                     ; If Direct mode
    ld      a,(BASYSCTL)
    and     $FF-SCRCOLOR
    or      c
    ld      (BASYSCTL),a
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
_clear_color
    jr      nz,_color_clear80
    jr      _color_clear40

screen_clear_a:
    ld      c,a
    ld      a,(LINLEN)
    cp      40                    ; NZ = 80 columns
    ld      a,c
    jr      _clear_screen
;-----------------------------------------------------------------------------
; Clear Text Screen
; Input: A: Color
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
_fill40
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
_fill80
    ld      hl,SCREEN
    ld      bc,2000
    jp      sys_fill_mem

; Output: A = Colors, NZ = 80 columns
_screen_width_colors:
    ld      a,(LINLEN)
    cp      40                    ; NZ = 80 columns
    ld      a,(SCREENCTL)         ; 
    rla                           ; Carry = SCRCOLOR
    ld      a,DFLTATTRS
    ret     nc
    ld      a,(SCOLOR)
    ret

;-----------------------------------------------------------------------------
; Copy Current Screen to Paged RAM
; Input: A: Page
;        B: 1 = Character Matrix, 2 = Color Matrix, 3 = Both
;       DE: Address
; Clobbered: A,AF',BC,DE,HL
;-----------------------------------------------------------------------------
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


; Output: DE = Offset
; Clobbered: BC
_border_offset:
    ld      c,IO_VCTRL
    in      b,(c)
    bit     5,b
    jr      nz,.remapped          ; If Border not remapped
    ld      de,0                   ;   Return 0
    ret                           ; Else              
.remapped
    ld      de,1023
    bit     6,b                   ;   If 40-column mode
    ret     z                     ;     Return 1024
    ld      de,2047               ;   Else
    ret                           ;     Return 2047


border_reset:
    ld      a,' '
    call    set_border_chr
    call    _screen_width_colors
; Input A: Character
; Clobbered: A,BC,DE,HL
border_set_color:
    call    _border_offset
    jr      color_write_byte

; Input A: Character
; Clobbered: A,BC,DE,HL
border_get_color:
    call    _border_offset
;-----------------------------------------------------------------------------
; Read byte from Color RAM
; Input: DE: Address Offset
; Output: A: Byte
; Flags: Carry set if address out of range
;-----------------------------------------------------------------------------
oolor_read_byte:
    ld      c,0                   ; C = RWFlag (0 = Read)
    byte    $21                   ; LD HL, over LD C
;-----------------------------------------------------------------------------
; Write byte to Color RAM
; Input: A: Byte
;       DE: Address
; Flags: Carry set if address out of range
;-----------------------------------------------------------------------------
color_write_byte:
    ld      c,$FF                 ; C = RWFlag ($FF = Write)
_color_byte:
    call    _bdehl_max_ofs         ; B = Byte, DE = MaxOfs, HL = AdrOfs
    ex      af,af'                ; 'F = WidFlg
    rst     COMPAR                      
    ccf                           ; If Addr >= MaxOfs
    ret     c                     ;   Return Erroe
    ex      af,af'                ; F = WidFlg
    jr      nz,.readwrite80       ; If 40 columns
    ld      de,$3400              ;   Convert AdrOfs to Color address
    jr      _readwrite_hlde       ;   Read or write byte   
.readwrite80    
    in      a,(IO_VCTRL)
    or      VCTRL_TEXT_PAGE       
    out     (IO_VCTRL),a          ; Select color page
    ex      af,af'                ; A' = VCTRL
    call    _readwrite_hl3000     ;   Read or write byte   
    ex      af,af'                ; A = VCTRL, A' = RdByte
    and     $FF-VCTRL_TEXT_PAGE   
    out     (IO_VCTRL),a          ;   Select screen page
    ex      af,af'                ; A = RdByte
    ret


; Input A: Character
; Clobbered: A,BC,DE,HL
set_border_chr:
    call    _border_offset
    jr      screen_write_byte

; Input A: Character
; Clobbered: A,BC,DE,HL
get_border_chr:
    call    _border_offset
;-----------------------------------------------------------------------------
; Read byte from screen
; Input: DE: Address Offset
; Output: A: Byte
; Flags: Carry set if address out of range
;-----------------------------------------------------------------------------
screen_read_byte:
    ld      c,0                   ; C = RWFlag (0 = Read)
    byte    $21                   ; LD HL, over LD C
;-----------------------------------------------------------------------------
; Write byte to screen
; Input: A: Byte
;       DE: Address Offset
; Flags: Carry set if address out of range
; Clobbered: A,B,DE.HL
;-----------------------------------------------------------------------------
screen_write_byte:
    ld      c,$FF                 ; C = RWFlag ($FF = Write)
_screen_byte:
    call    _bdehl_max_ofs         ; B = Byte, DE = MaxOfs, HL = AdrOfs
    ex      af,af'                ; 'F = WidFlg
    rst     COMPAR                ; If Addr >= MaxOfs      
    ccf                           ;   Return Carry set
    ret     c
    ex      af,af'                ; F = WidFlg
; If C=0, B -> (HL+$3000) Else (HL+$3000) -> A
_readwrite_hl3000:
    ld      de,$3000
; If C=0, B -> (HL+DE) Else (HL+DE) -> A
_readwrite_hlde:
    add     hl,de                 ; HL = ScrOfs + AdrOfs
    inc     c                     ; Z = Write, NZ = Read
    jr      nz,.read_byte
    ld      (hl),b                ; Write the byte
    ret
.read_byte
    ld      a,(hl)
    ret

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Read multiple bytes from Color RAM
; Input: BC: Number of bytes to read
;        DE: Color RAM Offset
;        HL: Destination Address
; Flags: Carry set if bytes will overflow Color RAM
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
color_read_bytes:
    xor     a                     ; Z = Read
    jr      _color_string
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Write multiple bytes to Color RAM
; Input: BC: Number of bytes to read
;        DE: Color RAM Offset
;        HL: Source Address
; Flags: Carry set if bytes will overflow Color RAM
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
color_write_bytes:
    or      $FF                   ; NZ = Write
_color_string:
    push    af                    ; Stack = RWFlag, RtnAdr
    push    hl                    ; Stack = StrAdr, RWFlag, RtnAdr
    push    de                    ; Stack = ScrOfs, StrAdr, RWFlag, RtnAdr
    call    _check_offset_len     ; DE = MaxOfs, HL = Scr0Ofs
    jp      c,discard3ret         ; If overflow, clean stack and return carry
    jp      z,discard3ret         ; If null string, pop RW flag and return
    ex      af,af'                ; F = WidFlg
    pop     hl                    ; HL = ScrOfs; Stack = StrAdr, RWFlag, RtnAdr
    ld      de,$3400              ; Default Base to $3400
    jr      z,.not80              ; If 80cols
    ld      d,$30                 ;   Base = $3000
.not80
    ex      af,af'                ; F' = WidFlg
    add     hl,de                 ; HL = ScrAdr
    ex      de,hl                 ; DE = ScrAdr
    pop     hl                    ; HL = StrAdr; Stack = RWFlag, RtnAdr
    pop     af                    ; AF = RWFlag, Stack = RtnAdr
    jr      nz,.write
;read
    ex      de,hl                 ; DE = StrAdr, HL = ScrAdr
.write
    ex      af,af'                ; F = WidFlg
    jp      nz,.write80           ; If 40cols
    ldir                          ;   Write it
    ret                           ; Else
.write80
    in      a,(IO_VCTRL)
    or      VCTRL_TEXT_PAGE       
    out     (IO_VCTRL),a          ;   Select color page
    ldir                          ;   Write the string
    and     low(~VCTRL_TEXT_PAGE)
    out     (IO_VCTRL),a          ;   Select screen page
    ret

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Read multiple bytes from Screen RAM
; Input: BC: Number of bytes to read
;        DE: Screen RAM Offset
;        HL: Destination Address
; Flags: Carry set if bytes will overflow Screen RAM
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
screen_read_bytes:
    xor     a                     ; Z = Read
    jr      _screen_string
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Write multiple bytes to Screen RAM
; Input: BC: Number of bytes to read
;        DE: Screen RAM Offset
;        HL: Source Address
; Flags: Carry set if bytes will overflow Screen RAM
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
screen_write_bytes:
    or      $FF                   ; NZ = Write
_screen_string:
    push    af                    ; Stack = RWFlag, RtnAdr
    push    hl                    ; Stack = StrAdr, RWFlag, RtnAdr
    push    de                    ; Stack = ScrOfs, StrAdr, RWFlag, RtnAdr
    call    _check_offset_len     ; DE = MaxOfs, HL = ScrOfs
    jp      c,discard3ret         ; If overflow, clean stack and return carry
    jp      z,discard3ret         ; If null string, pop RW flag and return
    pop     hl                    ; HL = ScrOfs; Stack = StrAdr, RWFlag, RtnAdr
    ld      de,$3000
    add     hl,de                 ; HL = MemAdr
    ex      de,hl                 ; DE = MemAdr
    pop     hl                    ; HL = StrAdr; Stack = RWFlag, RtnAdr
    pop     af                    ; AF = RWFlag; Stack = RtnAdr
    jr      nz,.write_string      ; If read
;read_string
    ex      de,hl                 ; DE = StrAdr, HL = MemAdr
.write_string
    ldir
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
screen_max_ofs:
    ld      de,1024               ; MaxOfs = 1024
    in      a,(IO_VCTRL)
    and     VCRTL_80COL_EN
    ret     z                     ; If 80 columns
    rl      d                     ;   MaxOfs = 2048
    or      a                     ; Set NZ flag
    ret
