;-----------------------------------------------------------------------------
; Fill Color Map Rectangle with Byte
; Takes bloxel/pixel coordinates as input
; Input: A: Reserved (Set to 0)
;       BC: Start Column
;       DE: Start Row
;      BC': End Column
;      DE': End Row
;        H: Reserved (Set to 0)
;        L: Combined Colors
; Clobbered: All
;-----------------------------------------------------------------------------
colormap_fill_rect:
    push    hl                    ; Stack = AndMsk+Colors, RtnAdr
    pop     ix                    ; IXh = AndMsk, IXl = Colors; Stack = RtnAdr
    ld      a,(GFX_FLAGS)
    and     GFXM_MASK
    call    colormap_scale_rect
    ret     m
    ret     c
    cp      2
    jr      z,.colormap           ; If Bloxels
    or      a
    ld      a,40
    jp      z,do_color_fill
    ld      a,80
    jp      do_color_fill         ; Else
.colormap    
    in      a,(IO_BANK1)
    push    af
    ld      a,VIDEO_RAM
    out     (IO_BANK1),a
    ld      a,40
    ld      iy,BANK1_BASE+BMP_COLORRAM
    call    fill_rect
    pop     af
    out     (IO_BANK1),a
    xor     a
    ret
    
;-----------------------------------------------------------------------------
; Convert Color Map Coordinates to Size and Start Address
; Input: A: GfxMode
;       BC: Start X
;       DE: End Y
;      BC': Start X
;      DE': End Y
; Output: BC: Start Col
;         DE: Start Row
;        BC': Start X
;        DE': End Y
; Clobbered: HL
;-----------------------------------------------------------------------------
colormap_scale_rect:
    cp      4
    jr      c,.convert
    or      $FF                   ; If GfxMode > 3
    ret                           ;   Return Minus
.convert
    call    bitmap__check_rect
    ret     c
    cp      2
    jr      z,.pixel_rect         ; If not 1bpp
    exx                           ;   BC = X2, DE = Y2, BC' = X1, DE' = Y1
    call    .bloxel_convert       ;   BC = EndCol, DE = EndRow
    exx                           ;   BC = Y1, DE = Y2, BC = EndCol, DE = EndRow
.bloxel_convert
    srl     b
    rr      c                     ;   BC = StartCol
    ld      hl,gfx_bloxel_row
    add     hl,de
    ld      e,(hl)
    ld      d,0                   ;   DE = StartRow
    ret                           ; Else
.pixel_rect:
    exx                           ;   BC = X2, DE = Y2, BC' = X1, DE' = Y1
    call    .pixel_convert        ;   BC = EndCol, DE = EndRow
    exx                           ;   BC = Y1, DE = Y2, BC = EndCol, DE = EndRow
.pixel_convert:
    srl     b
    rr      c
    srl     c
    srl     c                     ;   Column = X / 8
    srl     d
    rr      e
    srl     e
    srl     e                     ;   Row = Y / 8
    or      a                     ;   Clear Carry after shifts
    dec     de                    ;   Adjust for Bloxel Y Offset
    ret

; In: A = GfxMode, D=Column, E=Row
; Out: Carry set if out of bounds
colormap_bounds:
    cp      GFXM_WIDE
    ld      a,40
    jr      nz,.not80
    add     a,a
.not80
    cp      d                     ; If EndCol > 39
    ret     c                     ;   Return Carry Set
    ld      a,25
    cp      e                     ; If EndRow > 24
    ret                           ;   Return Carry Set

