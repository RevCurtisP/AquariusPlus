;=============================================================================
; Tile Assembly Routines
;=============================================================================

;-----------------------------------------------------------------------------
; Set tile pixel
; Input: C: Color Index
;        E: Pixel Index
;        HL: Tile #
; Clobbered: A,DE,HL
;-----------------------------------------------------------------------------
tile_set_pixel:
    push      hl
    call      _get_set_init       ; HL = TileAddr, A = Video RAM
    ld        d,0
    add       hl,de               ; HL = Pixel Address
    ex        de,hl               ; DE = Pixel Address
    pop       hl
    jp        page_write_byte     ; Write pixel to tile

;-----------------------------------------------------------------------------
; Set tile data
; Input: BC: Data length
;        DE: Data address
;        HL: Tile #
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
tile_set:
    call      _get_set_init       ; HL = TileAddr
    ex        de,hl               ; DE = TileAddr, HL = Dat
    jp        page_write_bytes    ; Write data to tile
    
;-----------------------------------------------------------------------------
; Get tile data
; Input: HL: Tile #
;        BC: Data length
;        DE: Data address
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
tile_get:
    call      _get_set_init       ; HL = TileAddr
    jp        page_read_bytes     ; Read data and return

_get_set_init:
    push      bc                  
    ld        b,5
    call      shift_hl_left       ; Calculate Tile Address
    ld        bc,TILE_DATA        ; 
    add       hl,bc
    pop       bc
    ld        a,VIDEO_RAM
    ret
    
;-----------------------------------------------------------------------------
; Set tilemap offset
; Input: BC: X-Offset
;         E: Y-Offset
; Clobbered: A
;-----------------------------------------------------------------------------
tilemap_set_offset:
    ld        a,c
    out       (IO_VSCRX_L),a
    ld        a,b
    out       (IO_VSCRX_H),a
    ld        a,e
    out       (IO_VSCRY),a
    ret
    
;-----------------------------------------------------------------------------
; Get tilemap offset
; Input: BC: X-Offset
;        DE: Y-Offset
; Clobbered: A
;-----------------------------------------------------------------------------
tilemap_get_offset:
    in        a,(IO_VSCRX_L)
    ld        c,a
    in        a,(IO_VSCRX_H)
    ld        b,a
    in        a,(IO_VSCRY)
    ld        e,a
    ld        d,0
    ret

;-----------------------------------------------------------------------------
; Combine Tile Properties
; Input: B: Attributes
;        C: Palette #
;       DE: Tile
; Output: B: Masked Attributes
;         C: Masked and shifted palette
;        DE: Combined Tile Entry
; Clobbered: A
;-----------------------------------------------------------------------------
tile_combine_props:
    ld      a,$03                 ; Mask palette#
    and     c
    rla                           ; Shift into position
    rla
    rla
    rla
    ld      c,a                   ; Back into c
    ld      a,$4E                 ; Mask Attributes
    and     b
    ld      b,a                   ; Back into b
    ld      a,$01                 ; Mask Tile MSb
    and     d
    or      b                     ; Add Attributes
    or      c                     ; Add Colors
    ld      d,a                   ; Back into d
    ret
    
;-----------------------------------------------------------------------------
; Write tile to tilemap
; Input: BC: 
;        DE: Y-Offset
;        HL: Tile + Properties
; Clobbered: A
;-----------------------------------------------------------------------------
tilemap_set_tile:

;-----------------------------------------------------------------------------
; Get tile from tilemap
; Input: C: Column
;        E: Row
; Output: BC = Tile + Properties
; Clobbered: A,DE
;-----------------------------------------------------------------------------
tilemap_get_tile:
    call    tilemap_cell_addr     ; DE = Cell Addres
    ret     c                     ; Return if Error
    ld      a,VIDEO_RAM           ; Reading Video RAM
    jp      page_read_word        ; Read word and return
    
;-----------------------------------------------------------------------------
; Calculate tilemap cell address
; Input: C: Column
;        E: Row
; Output: DE = Cell Address
;         Carry Set if Bad Args
; Clobbered: A
;-----------------------------------------------------------------------------
tilemap_cell_addr:   
    ld      a,e                   
    ld      d,a
    ld      e,0                   ; DE = Row * 256
    srl     d
    rr      e                     ; DE = Row * 128
    ld      a,c 
    sll     a                     ; A = Column * 2
    and     e                     ; Clears carry
    ld      e,a                   ; DE = (Row*64+Column)*2
    ret

;-----------------------------------------------------------------------------
; Fill Tilemap Section with Tile + Palette + Attributes
; Input: B: Start Column
;        C: End Column  
;        D: Start Row
;        E: End Row
;       HL: Tile#+Props
; Clobbered: A, BC, DE
;-----------------------------------------------------------------------------
tilemap_fill:
    call    _tile_bounds          ; Check EndCol and EndRow
    ret     c
    ld      ix,tilemap_cell_addr
    call    gfx_convert_rect      ; A = RowCnt, BC = ColCnt, DE = FilAdr
    ret     c
.loop
    push    af                    ; Stack = RowCnt, RtnAdr
    push    bc                    ; Stack = ColCnt, RowCnt, RtnAdr
    push    de                    ; Stack = FilAdr, ColCnt, RowCnt, RtnAdr 
    ld      a,VIDEO_RAM
    call    page_fill_word        ; In: A=Page, BC=Count, DE=FilAdr, HL=Word
    pop     de                    ; DE = FilAdr; Stack = ColCnt, RowCnt, RtnAdr
    ex      de,hl                 ; HL = FilAdr, DE = TilPrp
    ld      bc,128                ; Row Width in Words
    add     hl,bc                 ; Add to FilAdr
    ex      de,hl                 ; DE = FilAdr, HL = TilPrp
    pop     bc                    ; BC = ColCnt; Stack = RowCnt, RtnAdr
    pop     af                    ; A = RowCnt; Stack = RtnAdr
    dec     a                     ; If all rows done
    ret     z                     ;   Return
    jr      .loop                 ; Else do next row

; In: C=Column, E=Row
; Out: Carry set if out of bounds
_tile_bounds:
    ld      a,64
    cp      b                     ; If EndCol > 63
    ret     c                     ;   Return Carry Set
    ld      a,32                  
    cp      e                     ; If EndRow > 31
    ret                           ;   Return Carry Set

