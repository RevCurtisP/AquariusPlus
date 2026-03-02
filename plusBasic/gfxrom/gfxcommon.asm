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
