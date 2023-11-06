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
    ld      a,(TTYWID)
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
    ld      de,(TTYWID)           ; DE = Screen width
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
    ld      a,(TTYWID)
    cp      b                     ; If EndCol > 39
    ret     c                     ;   Return Carry Set
    ld      a,25
    cp      e                     ; If EndRow > 24
    ret                           ;   Return Carry Set

;-----------------------------------------------------------------------------
; Copy Screen Buffer to Text Screen
; Input: A: BUFSCRN40 or SWPSCRN40
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_restore:
    push    hl                    ; Stack = ScrBuf, RtnAdr
    ld      ix,restore_screen_vars
    call    aux_rom_call
    pop     hl                    ; HL = ScrBuf, RtnAdr
    in      a,(IO_VCTRL)
    bit     6,a                   
    jr      nz,.restore80         ; If 40 column screen
    bit     7,a                   ;   If primary page
    ld      de,SCRN40BUF          ;     Write to primary buffer
    jr      z,.restore_screen        ;   Else 
    ld      de,SCRN41BUF          ;     Write to auxiliary buffer
    jr      .restore_screen          ; Else
.restore80    
    set     7,a                   ;   Select Color RAM
    ld      de,SCRN80BUF+2048     ;   and Write it     
    call    .restore_screen           
    in      a,(IO_VCTRL)
    res     7,a                   ;   Select Screen RAM
    ld      de,-4096              ;   and Write it
.restore_screen
    add     hl,de                 
    ld      a,SCR_BUFFR
    ld      bc,2048
    ld      de,SCREEN             ; Copying from Text and Color RAM
    jp      page_fast_read_bytes  ; Do it

;-----------------------------------------------------------------------------
; Copy Text Screen to Screen Buffer
; Input: A: Cursor buffer offset: BUFSCRN40 or SWPSCRN40
;       HL: Screen buffer offset: SCRN40BUF or SCRN40SWP 
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_save:
    push    hl                    ; Stack = ScrBuf, RtnAdr
    ld      ix,save_screen_vars
    call    aux_rom_call
    pop     hl                    ; HL = ScrBuf, RtnAdr
    in      a,(IO_VCTRL)
    bit     6,a                   
    jr      nz,.save80            ; If 40 column screen
    bit     7,a                   ;   If primary page
    ld      de,SCRN40BUF          ;     Write to primary buffer
    jr      z,.save_screen        ;   Else 
    ld      de,SCRN41BUF          ;     Write to auxiliary buffer
    jr      .save_screen          ; Else
.save80    
    set     7,a                   ;   Select Color RAM
    ld      de,SCRN80BUF+2048     ;   and Write it     
    call    .save_screen           
    in      a,(IO_VCTRL)
    res     7,a                   ;   Select Screen RAM
    ld      de,-4096              ;   and Write it
.save_screen
    add     hl,de                 
    ex      de,hl
    ld      a,SCR_BUFFR
    ld      bc,2048
    ld      hl,SCREEN             ; Copying from Text and Color RAM
    jp      page_fast_write_bytes ; Do it

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
; Swap Text Screen with Screen Buffer
; Input: A: Cursor buffer offset: BUFSCRN40 or SWPSCRN40
;       HL: Screen buffer offset: SCRN40BUF or SCRN40SWP 
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_swap:
    push    hl                    ; Stack = ScrBuf, RtnAdr
    ld      ix,swap_screen_vars
    call    aux_rom_call
    pop     hl                    ; HL = ScrBuf, RtnAdr
    in      a,(IO_VCTRL)
    bit     6,a                   
    jr      nz,.swap80            ; If 40 column screen
    bit     7,a                   ;   If primary page
    ld      de,SCRN40BUF          ;     Write to primary buffer
    jr      z,.swap_screen        ;   Else 
    ld      de,SCRN41BUF          ;     Write to auxiliary buffer
    jr      .swap_screen          ; Else
.swap80    
    set     7,a                   ;   Select Color RAM
    ld      de,SCRN80BUF+2048     ;   and Write it     
    call    .swap_screen           
    in      a,(IO_VCTRL)
    res     7,a                   ;   Select Screen RAM
    ld      de,-4096              ;   and Write it
.swap_screen
    add     hl,de                 
    ex      de,hl
    ld      a,SCR_BUFFR
    ld      bc,2048
    ld      hl,SCREEN             ; Copying from Text and Color RAM
    jp      page_mem_swap_bytes   ; Do it


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

