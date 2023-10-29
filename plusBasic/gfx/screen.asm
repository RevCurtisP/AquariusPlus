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
; Read Text Screen Section into Buffer
; Input: B: Start Column
;        C: End Column
;        D: Start Row
;        E: End Row
;        HL: Buffer Address
; Clobbered: A, BC, DE
;-----------------------------------------------------------------------------
screen_get:
    call    screen_convert_rect   ; A = RowCnt, C = ColCnt, DE = RowAdr
    ret     c
    ld      (hl),c                ; Buffer[0] = Columns
    inc     hl
    ld      (hl),a                ; Buffer[1] = Rows
    inc     hl
    ex      af,af'
.rows:
    push    de                    ; Stack = RowAdr, RtnAdr
    ld      b,c                   ; B = Column Countdown
.columns
    ld      a,(de)                ; A = Character
    ld      (hl),a                ; Put in buffer
    inc     hl
    set     2,d
    ld      a,(de)                ; A = Colors
    ld      (hl),a                ; Put in buffer
    inc     hl
    res     2,d
    inc     de
    djnz    .columns
    pop     de                    ; DE = RowAdr; Stack = RtnAdr
    ex      af,af'
    dec     a                     ; If no more rows
    ret     z                     ; Return
    ex      af,af'
    ld      a,40
    add     e
    ld      e,a
    ld      a,0
    adc     d
    ld      d,a                   ; DE = Next RowAdr
    jr      .rows

;-----------------------------------------------------------------------------
; Write Text Screen Section from Buffer
; Input: C: Column
;        E: Row
;       HL: Buffer Address
; Clobbered: A, BC, DE
;-----------------------------------------------------------------------------
screen_put:
    call    _screen_bounds
    ret     c
    call    screen_pos_addr       ; DE = RowAdr
    ld      c,(hl)                ; C = ColCnt
    inc     hl
    ld      a,(hl)                ; A = RowCnt
    inc     hl
    ex      af,af'
.rows:
    push    de                    ; Stack = RowAdr, RtnAdr
    ld      b,c                   ; B = Column Countdown
.columns
    ld      a,(hl)                ; A = Character
    ld      (de),a                ; Write it
    inc     hl
    ld      a,(hl)                ; A = Colors
    set     2,d
    ld      (de),a                ; Write it
    inc     hl
    res     2,d
    inc     de
    djnz    .columns
    pop     de                    ; DE = RowAdr; Stack = RtnAdr
    ex      af,af'
    dec     a                     ; If no more rows
    ret     z                     ; Return
    ex      af,af'
    ld      a,40
    add     e
    ld      e,a
    ld      a,0
    adc     d
    ld      d,a                   ; DE = Next RowAdr
    jr      .rows

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
    ld      a,40
    cp      b                     ; If EndCol > 39
    ret     c                     ;   Return Carry Set
    ld      a,25
    cp      e                     ; If EndRow > 24
    ret                           ;   Return Carry Set

;-----------------------------------------------------------------------------
; Copy Screen Buffer to Text Screen
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_restore:
    ld      a,SCR_BUFFR
    ld      bc,2048
    ld      de,SCREEN             ; Copying from Text and Color RAM
    ld      hl,SCRN40BUFF         ; Copying to Text Screen Buffer
    jp      page_fast_read_bytes  ; Do it

;-----------------------------------------------------------------------------
; Copy Text Screen to Screen Buffer
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_save:
    ld      a,SCR_BUFFR
    ld      bc,2048
    ld      de,SCRN40BUFF         ; Copying to Text Screen Buffer
    ld      hl,SCREEN             ; Copying from Text and Color RAM
    jp      page_fast_write_bytes ; Do it

;-----------------------------------------------------------------------------
; Swap Text Screen with Screen Buffer
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_swap:
    ld      a,SCR_BUFFR
    ld      bc,2048
    ld      de,SCRN40BUFF         ; Swapping Text Screen Buffer
    ld      hl,SCREEN             ; with Text and Color RAM
    jp      page_mem_swap_bytes   ; Do it

;-----------------------------------------------------------------------------
; Update VCTRL Register
; Input: C: Bit Pattern
;        B: Bit Mask 
; Returns: A: New VCTRL value
;          C: Bit Pattern
;-----------------------------------------------------------------------------
screen_set_vctrl:
    in      a,(IO_VCTRL)
    and     b
    or      c
    out     (IO_VCTRL),a
    ret

;-----------------------------------------------------------------------------
; Switch Screens
; Input: A: If 0, save screen RAM to respective buffer
;           Else restore screen to respective buffer
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_switch:
    ret
;;; ToDo: Finish this
    or      a                     ; Set flags on argument
    call    nz,.get_screen_vars   ; B = TTYPOS, C = CURCHR, DE = CURRAM
    ex      af,af'                ; AF' = Flags
    in      a,(IO_VCTRL)          ; AF = VCTRL
    bit     6,a                   ; 
    jr      nz,.switch80          ; If not in 80 column mode
    bit     7,a                   ;   If auxilary screen page
;;;ToDo: ld hl accordingly and call save_screen_vars

    ld      bc,2048               ;   Copy 2048
    ld      hl,SCREEN             ;   
    jr      nz,.switch41          ;   If RAM Page 1
    ld      de,SCRN40SWAP



.switch41
    ex      af,af'                ; AF = Flags, AF' = VCTRL
    ld      a,SCR_BUFFR           ; A = 39
    jr      nz,.restore40
    jp      page_fast_write_bytes 



.restore40
    ld      a,SCR_BUFFR
    ex      de,hl
    call    page_fast_read_bytes
    jr     .restore_screen_vars
   
.switch80
    ex      af,af'
    jr      nz,.restore80
    ex      af,af'
    set     7,a                   ; Select Color RAM
    ld      hl,SCRN80SWAP+2048
    call    .write80
    res     7,a                   ; Select Screen RAM
    ld      hl,SCRN80SWAP
.write80:
    out     (IO_VCTRL),a
    ld      bc,2048
    ld      de,SCRN80SWAP
    ld      hl,SCREEN
    push    af                    ; 
    ld      a,SCR_BUFFR
;    call    .save_screen_vars
    call    page_fast_write_bytes
    pop     af
    ret
.restore80
    ex      af,af'
    set     7,a                   ; Select Color RAM
    ld      hl,SCRN80SWAP
    call    .read80
    ld      hl,SCRN80SWAP
    res     7,a                   ; Select Screen RAM
    call    .read80
    jr      .restore_screen_vars

.read80:
    out     (IO_VCTRL),a
    ld      bc,2048
    ld      de,SCREEN
    push    af
    ld      a,SCR_BUFFR
    call    page_fast_read_bytes
    pop     af
    ret

.get_screen_vars:
    ex      af,af'
    ld      a,(TTYPOS)
    ld      b,a
    ld      a,(CURCHR)
    ld      c,a
    ld      de,(CURRAM)
    ex      af,af'
    ret
    
    
    
.restore_screen_vars:
