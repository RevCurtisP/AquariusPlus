;=============================================================================
; Text Screen Read/Write Assembly Routines
;=============================================================================

reset_bordermap:
    in      a,(IO_VCTRL)
    and     a,$FF-VCTRL_REMAP_BC
    out     (IO_VCTRL),a
    ret

; Input A: Character
; Clobbered: A,BC,DE,HL
border_get_color:
    call    border_offset
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
    call    _check_offset_len     ; DE = MaxOfs, HL = ScrOfs
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

border_reset:
    ld      a,' '
    call    border_set_chr
    call    screen_width_colors
; Input A: Character
; Clobbered: A,BC,DE,HL
border_set_color:
    call    border_offset
    jr      color_write_byte

; Input A: Character
; Clobbered: A,BC,DE,HL
border_set_chr:
    call    border_offset
    jr      screen_write_byte

; Input A: Character
; Clobbered: A,BC,DE,HL
border_get_chr:
    call    border_offset
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

; Output: A = Colors, BC = ScrWid, NZ = 80 columns
screen_width_colors:
    ld      a,(LINLEN)
    cp      40                    ; NZ = 80 columns
    ld      c,a
    ld      b,0
screen_colors:
    ld      a,(SCREENCTL)         ; 
    rla                           ; Carry = SCRCOLOR
    ld      a,DFLTATTRS
    ret     nc
    ld      a,(SCOLOR)
    ret

; Output: DE = Offset
; Clobbered: BC
border_offset:
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
