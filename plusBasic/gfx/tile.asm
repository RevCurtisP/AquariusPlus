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
gfx_set_tile_pixel:
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
gfx_set_tile:
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
gfx_get_tile:
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