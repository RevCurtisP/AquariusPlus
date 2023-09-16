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
; Inpuit: BC: X-Offset
;        DE: Y-Offset
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

