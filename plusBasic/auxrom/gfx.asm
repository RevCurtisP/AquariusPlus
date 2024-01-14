;=====================================================================================
; plusBASIC Graphics Module
;=====================================================================================

;-----------------------------------------------------------------------------
; Graphics System Variables - Offset from Page Start
;-----------------------------------------------------------------------------
BUFSCRN40 equ $00   ; TTYPOS 
            ; $01   ; CURCHR
            ; $02   ; CURRAM

BUFSCRN41 equ $04   ; TTYPOS
            ; $05   ; CURCHR
            ; $06   ; CURRAM

BUFSCRN80 equ $08   ; TTYPOS
            ; $09   ; CURCHR
            ; $0A   ; CURRAM

SWPSCRN40 equ $0C   ; TTYPOS 
            ; $0D   ; CURCHR
            ; $0E   ; CURRAM

SWPSCRN41 equ $10   ; TTYPOS
            ; $11   ; CURCHR
            ; $12   ; CURRAM
               
SWPSCRN80 equ $14   ; TTYPOS
            ; $15   ; CURCHR
            ; $16   ; CURRAM

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
gfx_convert_rect:
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
gfx_rect_size:
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
