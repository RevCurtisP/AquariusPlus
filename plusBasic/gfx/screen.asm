;=============================================================================
; Text Screen Assembly Routines
;=============================================================================



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
    