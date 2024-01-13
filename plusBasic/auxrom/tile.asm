;=============================================================================
; Tile Assembly Routines
;=============================================================================

;-----------------------------------------------------------------------------
; Set tile data
; Input: BC: Data length
;        DE: Data address
;        HL: Tile #
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
tile_set:
    call      tile_addr_page      ; HL = TileAddr
    ex        de,hl               ; DE = TileAddr, HL = Dat
    jp        page_write_bytes    ; Write data to tile
    
;-----------------------------------------------------------------------------
; Get tile data
; Input: HL: Tile #
;        BC: Read length
;        DE: Buffer address
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
tile_get:
    call      tile_addr_page      ; HL = TileAddr
    jp        page_read_bytes     ; Read data and return

tile_addr_page:
    push      bc                  
    ld        b,5
    call      shift_hl_left       ; Calculate Tile Address
    ld        bc,TILE_DATA         
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
; Input: C: Column
;        E: Row
;        HL: Tile + Properties
; Clobbered: A
;-----------------------------------------------------------------------------
tilemap_set_tile:
    call    tilemap_cell_addr     ; DE = Cell Addres
    ret     c
    ld      b,h
    ld      c,l                   ; BC = TilPrp
    ld      a,VIDEO_RAM
    jp      page__write_word      ; Write to Video RAM and return

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
    ld      a,e                   ; A = Row
    ld      d,a                   ; D = Row
    ld      e,0                   ; DE = Row * 256
    srl     d
    rr      e                     ; DE = Row * 128
    ld      a,c 
    sla     a                     ; A = Column * 2
    or      e                     ; Combine E and E, clearing carry
    ld      e,a                   ; DE = Row*128+Column*2
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
    call    tile_convert_rect     ; A = RowCnt, BC = ColCnt, DE = FilAdr
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


;-----------------------------------------------------------------------------
; Read TileMap Section into Buffer
; Input: B: Start Column
;        C: End Column  
;        D: Start Row
;        E: End Row
;        HL: Buffer Address
; Clobbered: A, BC, DE
;-----------------------------------------------------------------------------
tilemap_get:
    call    tile_convert_rect     ; A = RowCnt, C = ColCnt, DE = RowAdr
    ret     c
    ld      (hl),c                ; Buffer[0] = Columns
    inc     hl
    ld      (hl),a                ; Buffer[1] = Rows
    inc     hl
    ld      iy,page_read_bytes_ex
    jr      _tilemap_put_get
    
;-----------------------------------------------------------------------------
; Write TileMap Section from Buffer
; Input: B: Start Column
;        C: End Column  
;        D: Start Row
;        E: End Row
;        HL: Buffer Address
; Clobbered: A, BC, DE
;-----------------------------------------------------------------------------
tilemap_put:
    call    tilemap_cell_addr     ; DE = RowAdr
    ret     c
    ld      c,(hl)                ; ColCnt = Buffer[0]
    inc     hl
    ld      a,(hl)                ; RowCnt = Buffer[1] 
    inc     hl
    ld      iy,page_write_bytes
_tilemap_put_get:
    sla     c                     ; C = BytCnt = ColCnt*2
.loop
    push    af                    ; Stack = RowCnt, RtnAdr
    push    bc                    ; Stack = ColCnt, RowCnt, RtnAdr
    push    de                    ; Stack = RowAdr, ColCnt, RowCnt, RtnAdr 
    ld      a,VIDEO_RAM
    call    jump_iy               ; In:  A = Page, BC: BytCnt, DE: SrcAdr, HL: DstAdr
    pop     de                    ; DE = RowlAdr; Stack = ColCnt, RowCnt, RtnAdr
    ex      de,hl                 ; HL = RowlAdr, DE = TilPrp
    ld      bc,128                ; Row Width in Bytes
    add     hl,bc                 ; Add to RowAdr
    ex      de,hl                 ; DE = RowAdr, HL = TilPrp
    pop     bc                    ; BC = ColCnt; Stack = RowCnt, RtnAdr
    pop     af                    ; A = RowCnt; Stack = RtnAdr
    dec     a                     ; If all rows done
    ret     z                     ;   Return
    jr      .loop                 ; Else do next row


;-----------------------------------------------------------------------------
; Convert Tile Coordinates to Size and Start Address
; Input: B: Start Column
;        C: End Column  
;        D: Start Row
;        E: End Row
; Output: A = Row Count
;         C = Column Count
;        DE = Start Address
;-----------------------------------------------------------------------------
tile_convert_rect:
    call    _tilemap_bounds       ; Check EndCol and EndRow
    ret     c
    ld      ix,tilemap_cell_addr
    call    gfx_convert_rect      ; A = RowCnt, C = ColCnt, DE = RowAdr
    ret     


; In: C=Column, E=Row
; Out: Carry set if out of bounds
_tilemap_bounds:
    ld      a,64
    cp      b                     ; If EndCol > 63
    ret     c                     ;   Return Carry Set
    ld      a,32                  
    cp      e                     ; If EndRow > 31
    ret                           ;   Return Carry Set

