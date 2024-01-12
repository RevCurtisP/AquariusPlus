;=============================================================================
; Text Screen Graphics Assembly Routines
;=============================================================================

;-----------------------------------------------------------------------------
; Read Text Screen Section into Buffer
; Input: A: Mode: 1 = SCREEN, 2 = COLOR, 3 = Both
;        B: Start Column
;        C: End Column
;        D: Start Row
;        E: End Row
;        HL: Buffer Address
; Output: Zero Flag set if invalid mode 
; Clobbered: A, AF', BC, DE, HL, IX, IY
;--------------`---------------------------------------------------------------
screen_get:
    and     3                     ; If Mode is not 1, 2, or 3
    ret     z                     ; Return
    ex      af,af'                ; A' = Mode
    call    screen_convert_rect   ; A = RowCnt, C = ColCnt, DE = RowAdr
    ret     c
    ld      (hl),c                ; Buffer[0] = Columns
    inc     hl
    ld      (hl),a                ; Buffer[1] = Rows
    inc     hl
    ld      iy,_get
    jr      screen_put_get

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
    scf
    ret     z                     ; Return error
    ex      af,af'                ; A' = Mode
    call    screen_bounds
    ret     c
    call    screen_pos_addr       ; DE = RowAdr
    ld      c,(hl)                ; C = ColCnt
    inc     hl
    ld      a,(hl)                ; A = RowCnt
    inc     hl
    ld      iy,_put
screen_put_get:
    push    af                    ; Stack = RowCnt, RtnAdr
    in      a,(IO_VCTRL)
    and     VCRTL_80COL_EN
    ld      ix,.color40
    jr      z,.not80
    ld      ix,.color80
.not80
.rows:
    push    de                    ; Stack = RowAdr, RowCnt RtnAdr
    ld      b,c                   ; B = Column Countdown
.columns
    ex      af,af'                ; A = Mode
;    or      a
;    call    z,.tile
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
    jr      z,.done               ;   Finish up
    push    af                    ; Stack = RowCnt, RtnAdr
    ld      a,(LINLEN)
    add     e
    ld      e,a
    ld      a,0
    adc     d
    ld      d,a                   ; DE = Next RowAdr
    jr      .rows
.done
    inc     a                     ; Clear Zero flag
    ret

.screen
    ex      af,af'                ; A' = Mode
    call    jump_iy
    ex      af,af'                ; A = Mode
    ret

.color
    jp      (ix)
.color40
    ex      af,af'                ; A' = Mode
    set     2,d
    call    jump_iy
    res     2,d
    ex      af,af'                ; A = Mode
    ret
.color80
    ex      af,af'                ; A' = Mode
    push    bc
    ld      c,IO_VCTRL
    in      b,(c)
    set     7,b
    out     (c),b
    call    jump_iy
    res     7,b
    out     (c),b
    pop     bc
    ex      af,af'                ; A' = Mode
    ret

.tile
    ret

_get:
    ld      a,(de)                ; Copy from screen
    ld      (hl),a                ; to buffer
    inc     hl
    ret
    
_put:
    ld      a,(hl)                ; Copy from buffer
    ld      (de),a                ; to screen
    inc     hl
    ret


_get_tile:
    ret

_put_tile:
    ret

;-----------------------------------------------------------------------------
; Fill Text or Color Screen Rectangle with Byte
; Input: B: Start Column
;        C: End Column
;        D: Start Row
;        E: End Row
;        H: 0 = Screen, $FF = Color
;        L: Byte
; Clobbered: A, BC, DE
;-----------------------------------------------------------------------------
screen_fill:
    in      a,(IO_VCTRL)
    and     VCRTL_80COL_EN
    jr      z,.not80              ; If 80 column mode
    and     h                     ;   If Fill Screen
    jr      z,.fill               ;   Do the fill
;fill 80 column color
    in      a,(IO_VCTRL)          ; Else
    push    af                    ;   Save IO_VCTRL
    or      VCTRL_TEXT_PAGE       ;   Switch to color page
    out     (IO_VCTRL),a
    inc     h                     ;   AdrOfs = 0
    call    .fill
    pop     af
    out     (IO_VCTRL),a
    ret
.not80    
    or      h                     
    jr      z,.fill               ; If filling color
    ld      h,$04                 ;   AdrOfs = $0400
.fill    
    call    screen_convert_rect   ; A = RowCnt, C = ColCnt, DE = RowAdr
    ret     c
    ex      de,hl                 ; D = AdrOfs, E = Byte, HL = RowAdr
    ex      af,af'                ; A' = RowCnt
    ld      a,d
    add     h                     ; Add offset to RowAdr
    ld      h,a
    ld      a,c                   ; A = ColCnt
    ex      af,af'                ; A = RowCnt, A' = ColCnt
.loop
    push    hl                    ; Save RowAdr
    ex      af,af'                ; A = ColCnt, A' = RowCnt
    ld      b,a                   ; B = ColCnt
.row
    ld      (hl),e                ; Write Byte
    inc     hl
    djnz    .row                  ; Next Column
    ld      hl,LINLEN
    ld      c,(hl)                ; BC = Line Length
    pop     hl                    ; HL = RowAdr
    add     hl,bc                 ; HL = Next RowAdr
    ex      af,af'                ; A = RowCnt, A' = ColCnt
    dec     a
    jr      nz,.loop
    ret
   
;-----------------------------------------------------------------------------
; Set screen color page
; Output: BC = Color RAM length
;         HL = Color RAM address
;          A = 0 if 40 column mode, VCRTL_80COL_EN if 80 column mode
; Set Flags: Z if 40 column mode, NZ if 80 column mode
; Stack: Original IO_VCTRL
; Clobbers: A
;-----------------------------------------------------------------------------
_screen_color_page:
    pop     ix                   ; IX = RtnAdr
    ld      hl,COLOR             ; HL = 40 column color base address
    ld      bc,1000              ; BC = 40 column screen length
    in      a,(IO_VCTRL)          
    push    af                   ; Stack = OldVCTRL
    and     VCRTL_80COL_EN
    jr      z,.return            ; If 80-column mode
    ld      hl,SCREEN            ;   HL = 40 column color base address
    ld      bc,2000              ;   BC = 40 column screen length
    in      a,(IO_VCTRL)         ; Switch to color page
    or      VCTRL_TEXT_PAGE      ; Switch to color page
    out     (IO_VCTRL),a
    or      a
.return 
    jp      (ix)

_screen_restore_page:
    pop     af
    out     (IO_VCTRL),a
    xor     a
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
; Clobbered: IX
;-----------------------------------------------------------------------------
screen_convert_rect:
    call    screen_bounds         ; Check EndCol and EndRow
    ret     c
    ld      ix,screen_pos_addr
    call    aux_convert_rect      ; A = RowCnt, C = ColCnt, DE = RowAdr
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
screen_bounds:
    ld      a,(LINLEN)
    cp      b                     ; If EndCol > 39
    ret     c                     ;   Return Carry Set
    ld      a,25
    cp      e                     ; If EndRow > 24
    ret                           ;   Return Carry Set

; Out: B = BgnCol, C = EndCol, D = BgnRow, E= EndRow
screen_size:
    push    af
    ld      a,(LINLEN)
    dec     a
    ld      c,a
    ld      e,24
    xor     a
    ld      b,a
    ld      d,a
    pop     af
    ret
    
