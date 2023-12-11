;=============================================================================
; Text Screen Assembly Routines
;=============================================================================

;-----------------------------------------------------------------------------
; Fill Text Screen Section with Character+Color
; Input: B: Start Column
;        C: End Column
;        D: Start Row
;        E: End Row
;        H: Character
;        L: Color
; Clobbered: A, BC, DE
;-----------------------------------------------------------------------------
screen_fill:
    call    screen_convert_rect   ; A = RowCnt, C = ColCnt, DE = RowAdr
    ret     c
    ex      de,hl                 ; D = Char, E = Color, HL = RowAdr
    ex      af,af'                ; A' = RowCnt
    ld      a,c                   ; A = ColCnt
    ex      af,af'                ; A = RowCnt, A' = ColCnt
.loop
    push    hl                    ; Save RowAdr
    ex      af,af'                ; A = ColCnt, A' = RowCnt
    ld      b,a                   ; B = ColCnt
.row
    ld      (hl),d                ; Write Char
    set     2,h
    ld      (hl),e                ; Write Color
    res     2,h
    inc     hl
    djnz    .row                  ; Next Column
    pop     hl                    ; HL = RowAdr
    ld      bc,40
    add     hl,bc                 ; HL = Next RowAdr
    ex      af,af'                ; A = RowCnt, A' = ColCnt
    dec     a
    jr      nz,.loop
    ret

;-----------------------------------------------------------------------------
; Fill color memory with value
; Input: B: Character
; Destroys: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_fill_chr:
   ld       hl,SCREEN
   ld       a,b
   jr       _screen_fill
   
;-----------------------------------------------------------------------------
; Fill color memory with value
; Input: C: Color Byte
; Destroys: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_fill_color:
   ld       hl,COLOR
   ld       a,c
_screen_fill:
   ld       bc,1000
   jp       sys_fill_mem
   
;-----------------------------------------------------------------------------
; Read Text Screen Section into Buffer
; Input: A: Mode: 1 = SCREEN, 2 = COLOR, 3 = Both
;        B: Start Column
;        C: End Column
;        D: Start Row
;        E: End Row
;        HL: Buffer Address
; Output: Zero Flag set if invalid mode 
; Clobbered: A, BC, DE, HL
;--------------`---------------------------------------------------------------
screen_get:
    and     3                     ; If Mode is not 1, 2, or 3
    ret     z                     ; Return error
    ex      af,af'                ; A' = Mode
    call    screen_convert_rect   ; A = RowCnt, C = ColCnt, DE = RowAdr
    ret     c
    ld      (hl),c                ; Buffer[0] = Columns
    inc     hl
    ld      (hl),a                ; Buffer[1] = Rows
    inc     hl
    push    af                    ; Stack = RowCnt, RtnAdr
.rows:
    push    de                    ; Stack = RowAdr, RowCnt RtnAdr
    ld      b,c                   ; B = Column Countdown
.columns
    ex      af,af'                ; A = Mode
    bit     0,a                   ; If 1 or 3
    call    nz,.screen            ;   Get Screen byte
    bit     1,a                   ; If 2 or 3
    call    nz,.color             ;   Get Color byte
    ex      af,af'                ; A = Mode
    inc     de
    djnz    .columns
    pop     de                    ; DE = RowAdr; Stack = RowAdr, RtnAdr
    pop     af                    ; A = RowCnt; Stack = RtnAdr
    dec     a                     ; If no more rows
    ret     z                     ; Return
    push    af                    ; Stack = RowCnt, RtnAdr
    ld      a,40
    add     e
    ld      e,a
    ld      a,0
    adc     d
    ld      d,a                   ; DE = Next RowAdr
    jr      .rows

.screen
    ex      af,af'                ; A' = Mode
    ld      a,(de)                ; A = Character
    ld      (hl),a                ; Put in buffer
    inc     hl
    ex      af,af'                ; A = Mode
    ret

.color
    ex      af,af'                ; A' = Mode
    set     2,d
    ld      a,(de)                ; A = Colors
    ld      (hl),a                ; Put in buffer
    inc     hl
    res     2,d
    ex      af,af'                ; A = Mode
    ret
;-----------------------------------------------------------------------------
; Write Text Screen Section from Buffer
; Input: A: Mode: 1 = SCREEN, 2 = COLOR, 3 = Both
;        C: Column
;        E: Row
;       HL: Buffer Address
; Clobbered: A, BC, DE
;-----------------------------------------------------------------------------
screen_put:
    and     3                     ; If Mode is not 1, 2, or 3
    ret     z                     ; Return error
    ex      af,af'                ; A' = Mode
    call    _screen_bounds
    ret     c
    call    screen_pos_addr       ; DE = RowAdr
    ld      c,(hl)                ; C = ColCnt
    inc     hl
    ld      a,(hl)                ; A = RowCnt
    inc     hl
    push    af                    ; Stack = RowCnt, RtnAdr
.rows:
    push    de                    ; Stack = RowAdr, RowCnt RtnAdr
    ld      b,c                   ; B = Column Countdown
.columns
    ex      af,af'                ; A = Mode
    bit     0,a                   ; If 1 or 3
    call    nz,.screen            ;   Get Screen byte
    bit     1,a                   ; If 2 or 3
    call    nz,.color             ;   Get Color byte
    ex      af,af'                ; A = Mode
    inc     de
    djnz    .columns
    pop     de                    ; DE = RowAdr; Stack = RowAdr, RtnAdr
    pop     af                    ; A = RowCnt; Stack = RtnAdr
    dec     a                     ; If no more rows
    ret     z                     ; Return
    push    af                    ; Stack = RowCnt, RtnAdr
    ld      a,(LINLEN)
    add     e
    ld      e,a
    ld      a,0
    adc     d
    ld      d,a                   ; DE = Next RowAdr
    jr      .rows

.screen
    ex      af,af'                ; A' = Mode
    ld      a,(hl)                ; A = Character
    ld      (de),a                ; Write it
    inc     hl
    ex      af,af'                ; A = Mode
    ret

.color
    ex      af,af'                ; A' = Mode
    set     2,d
    ld      a,(hl)                ; A = Colors
    ld      (de),a                ; Write it
    inc     hl
    res     2,d
    ex      af,af'                ; A = Mode
    ret

;-----------------------------------------------------------------------------
; Convert Screen Coordinates to Size and Start Address
; Input: B: Start Column
;        C: End Column
;        D: Start Row
;        E: End Row
; Output: A = Row Count
;         C = Column Count
;        DE = Start Address
;-----------------------------------------------------------------------------
screen_convert_rect:
    call    _screen_bounds        ; Check EndCol and EndRow
    ret     c
    ld      ix,screen_pos_addr
    call    gfx_convert_rect      ; A = RowCnt, C = ColCnt, DE = RowAdr
    ret

;-----------------------------------------------------------------------------
; Calculate screen position address
; Input: C: Column
;        E: Row
; Output: DE = Cell Address
; Clobbered: A
;-----------------------------------------------------------------------------
screen_pos_addr:
    push    hl
    push    bc
    ld      a,e                   ; A = Row
    ld      de,(LINLEN)           
    ld      d,0                   ; DE = Screen width
    call    mult_a_de             ; HL = Row Address, BC = Column, A = 0
    add     hl,bc                 ; Add column address
    ld      bc,SCREEN             ; Add to Text Screen base address
    add     hl,bc
    ex      de,hl                 ; DE = Cursor address
    pop     bc
    pop     hl
    ret

    ld      a,e
    add     a,a
    add     a,a
    add     a,e
    ld      e,a
    ld      d,0                   ; DE = Row * 5
    ex      de,hl                 ; HL = Row * 5
    add     hl,hl
    add     hl,hl
    add     hl,hl                 ; HL = Row * 40
 
    push    bc
    ld      b,0                   ; BC = Column
    add     hl,bc                 ; HL = Row * 40 + Column
    ld      bc,SCREEN             ; Screen character-matrix (= 12288 dec)
    add     hl,bc                 ; HL = Cell Address
    pop     bc
    ex      de,hl                 ; DE = Cell Address
    ret

; In: C=Column, E=Row
; Out: Carry set if out of bounds
_screen_bounds:
    ld      a,(LINLEN)
    cp      b                     ; If EndCol > 39
    ret     c                     ;   Return Carry Set
    ld      a,25
    cp      e                     ; If EndRow > 24
    ret                           ;   Return Carry Set


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
    push    af                    
    pop     ix                    ; IX = RdWrt
    push    de                    ; Stack = AdrOfs, RdWrt, RtnAdr
    call    string_addr_len       ; BC = StrLen, DE = StrAdr
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
    