;=====================================================================================
; plusBASIC Graphics Module
;=====================================================================================


;-----------------------------------------------------------------------------
; Initialize graphics variables
;-----------------------------------------------------------------------------
gfx_initvars:
    call    init_screen_buffers
    call    init_screen_buffers
    call    init_screen_vars
    ld      iy,bitmap_init_screen
    jp      gfxrom_call

;-----------------------------------------------------------------------------
; Set bitmap drawing mode
;  Input: A: Mode (0: bloxel, 2: 1bpp, 3: 4bpp)
; Output: A: New GFX_FLAGS
;-----------------------------------------------------------------------------
gfx_set_mode:
    and     GFXM_MASK             ; Isolate mode bits
    push    bc
    ld      b,a                   ; B = Mode
    ld      a,(GFX_FLAGS)         ; A = Extended Flags
    and     $FF-GFXM_MASK         ; Clear old mode
    or      b                     ; Set new modebit
    pop     bc
    ret

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

