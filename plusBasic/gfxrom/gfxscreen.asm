;=============================================================================
; Text Screen Graphics Assembly Routines
;=============================================================================


;-----------------------------------------------------------------------------
; Draw Rectangle on Text Screen
; Input: BC: Start Column
;        DE: Start Row
;        BC': End Column
;        DE': End Row
;        HL: DrawChars Address
;        HL': DrawColors Address
; Output: Carry set if coordinates out of range
; Clobbered: A, BC, DE, HL, BC', DE', HL'
;--------------`---------------------------------------------------------------
screen__rect:
    exx                           ; HL = DrwClrs
    push    hl                    ; Stack = DrwClrs, RtnAdr
    exx
    push    hl                    ; Stack = DrwChrs, DrwClrs, RtnAdr
    call    .convert_rect         ; B = RowCnt-2, C = ColCnt-2, HL = RowAdr
    pop     de                    ; DE = ChrAdr; Stack = DrwClrs, RtnAdr
    jp      c,POPHRT              ; Discard DrwClrs and Return if error
    push    bc                    ; Stack = Counts, DrwClrs, RtnAdr
    push    hl                    ; Stack = RowAdr, Counts, DrwClrs, RtnAdr
    call    .do_rect              ; Draw Rectangle Characters
    pop     hl                    ; HL = RowAdr, Stack = Counts, DrwClrs, RtnAdr
    pop     bc                    ; BC = Counts; Stack = DrwClrs, RtnAdr
    pop     de                    ; DE = DrwClrs; Stack = RtnAdr
    ld      a,(LINLEN)
    cp      40
    jr      z,.do40               ; If 80 columns
    in      a,(IO_VCTRL)
    push    af
    or      VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a          ;   Switch to Color RAM
    call    .do_rect
    pop     af
    out     (IO_VCTRL),a          ;   Restore VCTRL
    ret
.do40
    ld      a,h
    add     a,$04
    ld      h,a                   ; ScrAdr += $4000
.do_rect
    call    .doline               ; Draw top line
.dolines
    push    de
    call    .doline               ; Draw a line
    pop     de
    dec     c
    jr      nz,.dolines
    inc     de
    inc     de
    inc     de
.doline:
    push    bc                    ; Stack = RowCnt+ColCnt, RtnAdr
    push    hl                    ; Stack = RowAdr, RowCnt+ColCnt, RtnAdr
    ld      a,(de)                ; A = UpperLeft
    ld      (hl),a                ; Write to screen
    inc     hl
    inc     de
    ld      a,(de)                ; A = UpperMiddle
.lineloop
    ld      (hl),a
    inc     hl
    djnz    .lineloop
    inc     de
    ld      a,(de)                ; A = UpperRight
    ld      (hl),a                ; Write to screen
    inc     de                    ; Bump ChrPtr
    pop     hl                    ; HL = RowAdr; Stack = RowCnt+ColCnt, RtnAdr
    ld      bc,(LINLEN)
    ld      b,0
    add     hl,bc                 ; HL = Next RowAdr
    pop     bc                    ; BC = RowCnt+ColCnt, Stack = RtnAdr
    ret

.convert_rect
    call    screen__convert_rect  ; BC = RowCnt, DE = ColCnt, HL = ScrAdr
    ret     c
    ld      a,c                   ; A = ColCnt
    cp      3                     ; If ColCnt < 3
    ret     c                     ;   Return Carry Set
    dec     a
    dec     a
    ld      b,a                   ; B = ColCnt-2
    ld      a,e                   ; A = RowCnt
    cp      3                     ; Set Carry if RowCnt < 3
    dec     a
    dec     a
    ld      c,a                   ; C = RowCnt-2
    ret

; In: BC=Column, DE=Row
; Out: Carry set if out of bounds
; Clobbered: HL
screen__bounds:
    ld      hl,(LINLEN)
    ld      h,0
    dec     hl
    sbc     hl,bc                 ; If Column >= LinLen
    ret     c                     ;   Return Carry Set
    ld      hl,23
    sbc     hl,de                 ; If Row > 23
    ret                           ;   Return Carry Set

;-----------------------------------------------------------------------------
; Convert Screen Coordinates to Size and Start Address
; Input: BC: Start Column
;        DE: Start Row
;        BC': End Column
;        DE': End Row
; Output: BC = Column Count
;         DE = Row Count
;         HL = Start Address
;  Flags: Carry set if out of bounds
; Clobbered: HL
;-----------------------------------------------------------------------------
screen__convert_rect:
    exx                           ; BC = EndCol, DE = EndRow, BC' = BgnCol, DE' = BgnRow
    call    screen__bounds        ; If out of bounds
    ret     c                     ;   Return Carry Set
    call    exx_check_rect        ; BC' = ColCnt, DE' = RowCnt
    ret     c                     ;   Return Carry Set
    call    screen__pos_addr      ; HL = ScrAdr
    push    hl                    ; Stack = ScrAdr, RtnAdr
    exx                           ; BC = ColCnt, DE = RowCnt
    pop     hl                    ; HL - ScrAdr; Stack = RtnAdr
    ret

; Input: BC: Start Column, DE: Start Row, BC': End Column, DE': End Row
; Output: BC': Column Count, DE'; Row Count
; Enter with Carry Clear
; Clobbers: HL
gfx_check_rect:
    exx                           ; BC = EndCol, DE = EndRow, BC' = BgnCol, DE' = BgnRow
exx_check_rect:
    push    bc                    ; Stack = EndCol, RtnAdr
    exx                           ; BC = BgnCol, DE = BgnRow, BC' = EndCol, DE' = EndRow
    pop     hl                    ; HL = EndCol; Stack = RtnAdr
    sbc     hl,bc                 ; If HL = EndCol - BgnCol
    ret     c                     ; If < 0, return Carry Set
    inc     hl                    ; HL = ColCnt
    push    hl                    ; Stack = ColCnt, RtnAdr
    exx                           ; BC = EndCol, DE = EndRow, BC' = BgnCol, DE' = BgnRow
    pop     bc                    ; BC = ColCnt; Stack = RtnAdr
    push    de                    ; Stack = EndRow, RtnAdr
    exx                           ; BC = BgnCol, DE = BgnRow, BC' = ColCnt, DE' = EndRow
    pop     hl                    ; HL = EndRow; Stack = RtnAdr
    sbc     hl,de                 ; HL = EndRow - BgnRow
    ret     c                     ; If < 0, return Carry Set
    inc     hl                    ; HL = RowCnt
    push    hl                    ; Stack = ColCnt, RtnAdr
    exx                           ; BC = ColCnt, DE = EndRow, BC' = BgnCol, DE' = BgnRow
    pop     de                    ; DE = RowCnt; Stack = RtnAdr
    exx                           ; BC = BgnCol, DE = BgnRow, BC' = ColCnt, DE' = RowCnt
    ret

;-----------------------------------------------------------------------------
; Calculate screen position address
; Input: C: Column
;        E: Row
; Output: HL = Cell Address
; Clobbered: A
;-----------------------------------------------------------------------------
screen__pos_addr:
    push    de
    push    bc
    inc     de                    ; Skip first row of screen
    ld      a,e                   ; A = Row
    ld      de,(LINLEN)           
    ld      d,0                   ; DE = Screen width
    call    mult_a_de             ; HL = Row Address, BC = Column, A = 0
    add     hl,bc                 ; Add column address
    ld      bc,SCREEN             ; Add to Text Screen base address
    add     hl,bc                 ; HL = ScrAdr
    pop     bc
    pop     de
    ret

screen__colors:
    ld      a,(SCREENCTL)         ; 
    rla                           ; Carry = SCRCOLOR
    ld      a,DFLTATTRS
    ret     nc
    ld      a,(SCOLOR)
    ret
