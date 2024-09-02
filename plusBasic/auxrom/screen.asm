;=============================================================================
; Text Screen Assembly Routines
;=============================================================================

;-----------------------------------------------------------------------------
; Fill Color RAM with current/default colors
;-----------------------------------------------------------------------------
screen_clear_color:
    ld      a,(BASYSCTL)
    rla     
    ld      a,DFLTATTRS
    jr      nc,.skip
    ld      a,(SCOLOR)
.skip
    ld      c,IO_VCTRL
    in      b,(c)
    bit     6,b    
    jr      nz,.clear80
    ld      hl,COLOR
    ld      bc,1024
    jp      sys_fill_mem
.clear80
    set     7,b
    out     (c),b
    push    bc  
    ld      hl,SCREEN
    ld      bc,2048
    call    sys_fill_mem
    pop     bc
    res     7,b
    out     (c),b
    ret

;-----------------------------------------------------------------------------
; Copy Current Screen to Paged RAM
; Input: A: Page
;        DE: Address
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
    ex      de,hl                 ; HL - Address
    jr      _read_write_screen
screen_write_fast:
    ld      iy,_screen_to_paged_fast
    jr      _read_write_screen
screen_write_paged:
    ld      iy,_screen_to_paged
_read_write_screen:
    ld      c,IO_VCTRL
    in      b,(c)          
    bit     6,b                   ; If 40-colomn mode
    jp      z,jump_iy             ;   Write it and Return
    call    screen_textpage_0
    push    af                    ; Stack = Page, RtnAdr
    call    jump_iy               ; Write Screen RAM
    pop     af                    ; A = Page; Stack = RtnAdr
    call    screen_textpage_1
    call    jump_iy               ; Write Color RAM
    jr      screen_textpage_0
_screen_to_paged_fast:
    ld      ix,page_fast_write_bytes
    jr      _to_paged
_screen_to_paged:
    ld      ix,page_write_bytes
_to_paged:
    ld      hl,SCREEN
    jr      _copy
_paged_to_screen_fast:
    ld      ix,page_fast_read_bytes
    jr      _to_screen
_paged_to_screen:
    ld      ix,page_read_bytes
_to_screen:
    ld      de,SCREEN
_copy:
    ld      bc,2048
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

;-----------------------------------------------------------------------------
; Read byte from screen
; Input: DE: Address Offset
; Output: A: Byte
; Flags: Carry set if address out of range
;-----------------------------------------------------------------------------
screen_read_byte:
    ld      c,0                   ; C = RdWrt (0 = Read)
    jr      _screen_byte

;-----------------------------------------------------------------------------
; Write byte to screen
; Input: A: Byte
;       DE: Address Offset
; Flags: Carry set if address out of range
; Clobbered: A,B,DE.HL
;-----------------------------------------------------------------------------
screen_write_byte:
    ld      c,$FF                 ; C = RdWrt ($FF = Write)
_screen_byte:
    ld      b,a                   ; B = Byte
    call    screen_max_ofs_de     ; DE = MaxOfs, HL = AdrOfs
    rst     COMPAR                ; If Addr >= MaxOfs      
    ccf                           ;   Return Carry set
    ret     c
    ld      de,$3000
_readwrite_b_to_hl
    add     hl,de                 ; Convert HL to Screen address
    inc     c                     ; Z = Write, NZ = Read
    jr      nz,.read_byte
    ld      (hl),b                ; Write the byte
    ret
.read_byte
    ld      a,(hl)
    ret

;-----------------------------------------------------------------------------
; Write string to screen
; Input: DE: Address Offset
;        HL: String Descriptor
; Flags: Carry set if address out of range
;-----------------------------------------------------------------------------
screen_read_string:
    xor     a                     ; Z = Read
    push    af                    ; Stack = RdWrt, RtnAdr
    jr      _screen_string

;-----------------------------------------------------------------------------
; Write string to screen
; Input: DE: Address Offset
;        HL: String Descriptor
; Flags: Carry set if address out of range
;-----------------------------------------------------------------------------
screen_write_string:
    or      $FF                   ; NZ = Write
    push    af                    ; Stack = RdWrt, RtnAdr
_screen_string:
    push    de                    ; Stack = AdrOfs, RdWrt, RtnAdr
    call    string_addr_len       ; BC = StrLen, DE = StrAdr
    jp      z,discard2ret         ; If null string, clear stack and return
    pop     hl                    ; HL = AdrOfs; Stack = RdWrt, RtnAdr
    push    de                    ; Stack = StrAdr, RdWrt, RtnAdr
    push    hl                    ; Stack = AdrOfs, StrAdr, RdWrt, RtnAdr
    call    screen_max_ofs_hl     ; DE = MaxOfs, HL = AdrOfs
    add     hl,bc                 ; HL = AdrOfs + StrLen
    dec     hl                    ; HL = AdrOfs + StrLen - 1
    rst     COMPAR                ; If StrEnd >= MaxOfs      
    ccf                           ;   Set Carry
    ex      af,af'                ; AF' = ErrFlg
    pop     hl                    ; HL = AdrOfs; Stack = StrAdr, RdWrt, RtnAdr
    ld      de,$3000
    add     hl,de                 ; HL = MemAdr
    ex      de,hl                 ; DE = MemAdr
    pop     hl                    ; HL = StrAdr; Stack = RdWrt, RtnAdr
    pop     af                    ; AF = RdWrt; Stack = RtnAdr
    ex      af,af'                ; AF = ErrFlg, AF' = RdWrt
    ret     c                     ; Return if Error now that Stack is cleared
    ex      af,af'                ; AF = RdWrt
    jr      nz,.write_string      ; If read
    ex      de,hl                 ; DE = StrAdr, HL = MemAdr
.write_string
    ldir
    ret
    
;-----------------------------------------------------------------------------
; Read byte from Color RAM
; Input: DE: Address Offset
; Output: A: Byte
; Flags: Carry set if address out of range
;-----------------------------------------------------------------------------
oolor_read_byte:
    ld      c,0                   ; C = RdWrt (0 = Read)
    jr      _color_byte

;-----------------------------------------------------------------------------
; Write byte to Color RAM
; Input: A: Byte
;       DE: Address
; Flags: Carry set if address out of range
;-----------------------------------------------------------------------------
color_write_byte:
    ld      c,$FF                 ; C = RdWrt ($FF = Write)
_color_byte:
    ld      b,a                   ; B = Byte
    call    screen_max_ofs_de     ; DE = MaxOfs, HL = AdrOfs
    ex      af,af'                ; AF' = Cols
    rst     COMPAR                ; If Addr >= MaxOfs      
    ccf                           ;   Return Erroe
    ret     c
    ex      af,af'                ; AF = Cols
    jr      nz,.readwrite80       ; If 40 columns
    ld      de,$3400              ;   Convert AdrOfs to Color address
    jr      _readwrite_b_to_hl    ;   and write byte   
.readwrite80    
    ld      de,$3000              ; Else
    add     hl,de                 ;   Convert HL to Base address
    in      a,(IO_VCTRL)
    or      VCTRL_TEXT_PAGE       
    out     (IO_VCTRL),a          ;   Select color page
    inc     c                     ;   Z = Write, NZ = Read
    jr      nz,.read80
    ld      (hl),b                ;   Write the byte
    jr      .done80
.read80
    ld      b,(hl)
.done80
    and     low(~VCTRL_TEXT_PAGE)
    out     (IO_VCTRL),a          ;   Select screen page
    ret


;-----------------------------------------------------------------------------
; Write string to screen
; Input: DE: Address Offset
;        HL: String Descriptor
; Flags: Carry set if address out of range
;-----------------------------------------------------------------------------
color_read_string:
    xor     a                     ; Z = Read
    jr      _color_string

;-----------------------------------------------------------------------------
; Write string to Color RAM
; Input: DE: Address Offset
;        HL: String Descriptor
; Flags: Carry set if address out of range
;-----------------------------------------------------------------------------
color_write_string:
    or      $FF                   ; NZ = Write
_color_string:
    push    af                    ; Stack = RdWrt, RtnAdr
    pop     ix                    ; IX = RdWrt
    push    de                    ; Stack = AdrOfs, RtnAdr
    call    string_addr_len       ; BC = StrLen, DE = StrAdr
    jp      z,discard_ret         ; If null string, clear stack and return
    pop     hl                    ; HL = AdrOfs; Stack = RdWrt, RtnAdr
    push    de                    ; Stack = StrAdr, RdWrt, RtnAdr
    push    hl                    ; Stack = AdrOfs, StrAdr, RdWrt, RtnAdr
    call    screen_max_ofs_hl     ; DE = MaxOfs, HL = AdrOfs
    ex      af,af'                ; F' = 80cols
    add     hl,bc                 ; HL = AdrOfs + StrLen
    dec     hl                    ; HL = AdrOfs + StrLen - 1
    rst     COMPAR                ; If StrEnd >= MaxOfs      
    ccf                           ;   Set Carry
    ex      af,af'                ; F = 80cols, F' = Error
    ld      de,$3400              ; Default Base to $3400
    jr      z,.not80              ; If 80cols
    ld      d,$30                 ;   Base = $3000
.not80
    pop     hl                    ; HL = AdrOfs; StrAdr, RtnAdr
    add     hl,de                 ; HL = DstAdr
    ex      de,hl                 ; DE = DstAdr
    pop     hl                    ; HL = StrAdr; Stack = RdWrt, RtnAdr
    ex      af,af'                ; F = Error, F' = 80cols
    ret     c                     ; Return if Error now that Stack is cleared
    push    ix
    pop     af                    ; AF = RdWrt
    jr      nz,.write
    ex      de,hl
.write
    ex      af,af'                ; F = 80cols
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

;-----------------------------------------------------------------------------
; Calculate Maximum Screen Offset
; Input: A: Byte
;       DE: Address
; Output: A: VCRTL_80COL_EN bit of IO_VCTRL
;        DE: Max Offset
;        HL: Address
; Flags: Zero cleared if 80 columns
;-----------------------------------------------------------------------------
screen_max_ofs_de:
    ex      de,hl                 ; HL = Addr
screen_max_ofs_hl:
    ld      de,1024               ; MaxOfs = 1024
    in      a,(IO_VCTRL)
    and     VCRTL_80COL_EN
    ret     z                     ; If 80 columns
    rl      d                     ;   MaxOfs = 2048
    ret
    