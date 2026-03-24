;-----------------------------------------------------------------------------
; Fill 1bpp Color Map Rectangle with Byte
; Input: B: Start Column
;        C: End Column
;        D: Start Row
;        E: End Row
;        L: Byte
; Clobbered: A, BC, DE, H
;-----------------------------------------------------------------------------
colormap_fill:
    push    hl                    ; Stack = Colors, RtnAdr
    ld      a,(GFX_FLAGS)
    and     GFXM_MASK
    ld      (BMPMODE),a
    cp      GFXM_4BPP
    ret     z
    call    colormap_convert_rect ; A = RowCnt, C = ColCnt, DE = RowAdr
    pop     hl                    ; L = Colors; Stack = RtnAdr
    ret     c
    ld      h,a                   ; H = RowCnt
    ld      a,(BMPMODE)
    or      a
    jr      z,_fill_40
    dec     a
    jr      z,_fill_80
_fill_1bpp
    ld      a,VIDEO_RAM
    call    page_map_bank1        ; Stack = OrigPg, RtnAdr
    call    _fill_40
    jp      page_restore_bank1

_fill_80:
    in      a,(IO_VCTRL)
    push    af                   ; Stack = IO_VCTRL, RtnAdr
    or      VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a         ; Switch to Color RAM
    ld      a,80                 ; A = RowWid
    call    _fill_color
    pop     af                   ; A = OrigVCTRL; Stack = RtnAdr
    out     (IO_VCTRL),a         ; Switch to Color RAM
    ret

_fill_40:
    ld      a,40                  ; A = RowWid
_fill_color:
    ex      af,af'                ; A' = RowWid
    ld      a,h                   ; A = RowCnt
    ex      de,hl                 ; E = Byte, HL = RowAdr
    ex      af,af'                ; A = RowWid, A' = RowCnt
    ld      d,a                   ; D = RowWid
    ld      a,c                   ; A = ColCnt
    ex      af,af'                ; A = RowCnt, A' = ColCnt
.loop
    push    hl                    ; Stack = RowAdr, RtnAdr
    ex      af,af'                ; A = ColCnt, A' = RowCnt
    ld      b,a                   ; B = ColCnt
.row
    ld      (hl),e                ; Write Byte
    inc     hl
    djnz    .row                  ; Next Column
    pop     hl                    ; HL = RowAdr; Stack = RtnAdr
    ld      c,d
    add     hl,bc                 ; HL = Next RowAdr
    ex      af,af'                ; A = RowCnt, A' = ColCnt
    dec     a
    jr      nz,.loop
    ret

;-----------------------------------------------------------------------------
; Convert Color Map Coordinates to Size and Start Address
; Input: B: Start Column
;        C: End Column
;        D: Start Row
;        E: End Row
; Output: A = Row Count
;         C = Column Count
;        DE = Start Address
; Clobbered: IX
;-----------------------------------------------------------------------------
colormap_convert_rect:
    ld      a,(BMPMODE)
    call    colormap_bounds         ; Check EndCol and EndRow
    ret     c
    call    _ix_pos_addr
    call    gfxrom_convert_rect     ; A = RowCnt, C = ColCnt, DE = RowAdr
    ret

_ix_pos_addr:
    ld      a,(BMPMODE)
    ld      ix,_pos_addr_40
    or      a
    ret     z
    ld      ix,_pos_addr_80
    dec     a
    ret     z
    ld      ix,_pos_addr_1bpp
    ret
    
;-----------------------------------------------------------------------------
; Calculate screen position address
; Input: C: Column
;        E: Row
; Output: DE = Cell Address
; Clobbered: A
;-----------------------------------------------------------------------------
_pos_addr_1bpp:
    push    hl
    push    bc
    ld      a,e                   ; A = Row
    ld      de,40           
    call    mult_a_de             ; HL = Row Address, BC = Column, A = 0
    add     hl,bc                 ; Add column address
    ld      bc,BANK1_BASE+8192    ; Add to ColorMap base address
    add     hl,bc
    ex      de,hl                 ; DE = Cursor address
    pop     bc
    pop     hl
    ret

_pos_addr_40:
    push    hl
    call    screen__pos_addr      ; HL = ScrnAddr
    ld      de,1024
    add     hl,de                 ; HL = ColorAddr
    ex      de,hl                 ; DE = ColorAddr
    pop     hl
    ret

_pos_addr_80:
    push    hl
    call    screen__pos_addr      ; HL = ScrnAddr
    ex      de,hl                 ; DE = ColorAddr
    pop     hl
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

