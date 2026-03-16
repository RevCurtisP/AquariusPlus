;-----------------------------------------------------------------------------
; Convert rectangular coordinates to address, columns and row count
; Input: B: Start Column
;        C: End Column  
;        D: Start Row
;        E: End Row
;       IX: Cell Address calculation routine
; Output: A: Row Count
;        BC: Column Count
;        DE: Start Address
;-----------------------------------------------------------------------------
gfxrom_convert_rect:
    ld      a,c                   ; A = EndCol
    sub     b                     ; A = EndCol - BgnCol
    ret     c                     ; If EndCol < BgnCol Return error
    inc     a                     ; ColCnt = EndCol - BgnCol + 1
    ld      c,b                   ; C = BgnCol
    ld      b,a                   ; B = ColCnt
    ld      a,e                   ; A = EndRow
    sub     d                     ; A= EndRow - BgnRow
    ret     c                     ; If EndRow < BgnRow Return error
    inc     a                     ; A = RowCnt 
    push    af                    ; Stack = RowCnt, RtnAdr
    ld      e,d                   ; E = BgnRow
    call    jump_ix               ; A = 0, DE = BgnAdr
    ret     c                     ; Return if error
    ld      c,b                   
    ld      b,0                   ; BC = ColCnt
    pop     af                    ; A = RowCnt; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; Calculate total number of cells in a rectangle
; Input: B: Start Column
;        C: End Column  
;        D: Start Row
;        E: End Row
; Output: HL: Total number of cells
;     Flags: Carry set if more than 255 cells 
;-----------------------------------------------------------------------------
gfxrom_rect_size:
    push    de                    ; Stack = RowSE, RtnAdr
    ld      a,e
    sub     d
    inc     a
    ld      e,a
    ld      d,0                   ; E = EndRow - BgnRow + 1
    ld      a,c
    sub     b
    inc     a                     ; A = EndCol - BgnCol + 1
    call    mult_a_de             ; HL = Rows * Cols
    pop     de                    ; DE = RowSE; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; Input: A = RectCnt
;       BC = X
;       DE = Y
;       HL = ListAdr
; Output: A = RectIndex
;-----------------------------------------------------------------------------
gfx_in_rectlist:
    push    af                    ; Stack = RectCnt, RtnAdr
.loop
    push    hl
    call    gfx_in_rect
    pop     hl
    jr      z,.found_rect
    jr      nc,.found_rect
    inc     hl
    inc     hl
    inc     hl
    inc     hl
    inc     hl
    inc     hl
    inc     hl
    inc     hl
    dec     a
    jr      nz,.loop
    pop     hl                  ; Stack = RtnAdr
    ret   
.found_rect
    ld      b,a
    pop     af
    sub     b
    inc     a
    ret

;-----------------------------------------------------------------------------
;       BC = X
;       DE = Y
;       HL = Rect`Adr
;-----------------------------------------------------------------------------
gfx_in_rect:
    call    _check_x
    jr      z,.y1
    ccf
.y1
    ret     c                     ; If X < X1, return Carry Set
    call    _check_y
    jr      z,.y2
    ccf
    ret     c                     ; If Y < Y1, return Carry Set
.y2
    call    _check_x
    ret     c                     ; If X > X2, return Carry Set
    call    _check_y
    ret                     

_check_x:
    push    de                    ; Stack = Y, RtnAdr
    ld      e,(hl)
    inc     hl
    ld      d,(hl)
    inc     hl                    ; DE = X-bound
    ex      de,hl                 ; DE = RectPtr, HL = X-bound
    or      a
    sbc     hl,bc
    ex      de,hl                 ; HL = RectPtr
    pop     de                    ; DE = Y; Stack = RtnAdr
    ret

_check_y:
    push    de                    ; Stack = Y, RtnAdr
    ld      e,(hl)
    inc     hl
    ld      d,(hl)
    inc     hl                    ; DE = Y-bound
    ex      (sp),hl               ; HL = Y; Stack = RectPtr, RtnAdr
    ex      de,hl                 ; DE = Y, HL = Y-bound
    or      a
    sbc     hl,de
    pop     hl                    ; HL = RectPtr; Stack = RtnAdr
    ret

; Input: BC: BytCnt
copy_tmpbase_vidbase:
    ld      de,0
; Input: BC: BytCnt, DE: DstAdr
copy_tmpbase_vidram:
    ld      hl,0
; Input: BC: BytCnt, DE: DstAdr, HL: SrcAdr
; Output: DE: NewDstAdr, HL: NewSrcAdr
copy_tmpbfr_vidram:
    ld      a,TMP_BUFFR           ; Copying from buffer
    ex      af,af'
    ld      a,VIDEO_RAM           ; to Video RAM
    jp      page_fast_copy

