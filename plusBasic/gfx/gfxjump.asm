;-----------------------------------------------------------------------------
; Graphic Routines Jump Table
;-----------------------------------------------------------------------------
gfx_jump_table
    jp      gfx_startup           ; $8000
    jp      gfx_init              ; $8003
    jp      screen_set_mode       ; $8006 
    jp      palette_shift_num     ; $8009 
    jp      palette_set_entry     ; $800C 
    jp      palette_set           ; $800F
    jp      palette_get           ; $8012
    jp      tile_set_pixel        ; $8015
    jp      tile_set              ; $8018
    jp      tile_get              ; $801B
    jp      spritle_set_attr      ; $801E
    jp      sprite_set_attrs      ; $8021
    jp      spritle_set_palette   ; $8024
    jp      sprite_set_palettes   ; $8027
    jp      spritle_set_tile      ; $802A
    jp      sprite_set_tiles      ; $802D
    jp      spritle_get_attrs     ; $8030
    jp      sprite_get_attrs      ; $8033
    jp      sprite_toggle         ; $8036
    jp      spritle_toggle        ; $8039
    jp      spritle_toggle_all    ; $803C
    jp      sprite_set_pos        ; $803F
    jp      spritle_set_pos       ; $8042
    jp      spritle_clear         ; $8045
    jp      spritle_clear_all     ; $8048
    
