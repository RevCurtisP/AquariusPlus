; Graphics module jump table
;; ToDo: Add screen_aux routines as they are moved in from screen.asm
;; Comment all routines

_jump_table:
; .
; *****************
; * Graphics ROM  *
; * LD   IY,label *
; * CALL gfx_call *
; *****************
; gfxbitmap.asm <<Bitmap Graphics>>
    jp      bitmap_init_vars      ; Initialize bitmap system variables
    jp      bitmap_set_mode       ; Set bitmap mode system variable from video control register
    jp      bitmap_read_sysvars   ; Read bitmap system variables
    jp      bitmap_read_color     ; Get Bitmap Draw Colors
    jp      bitmap_write_color    ; Set Bitmap Draw Colors
    jp      bitmap_clear_screen   ; Clear Bitmap
    jp      bitmap_fill_byte      ; Fill Bitmap with Byte
    jp      bitmap_fill_color     ; Fill Bitmap Color RAM
    jp      bitmap_line           ; [Future]
    jp      just_ret              ; (bitmap_circle)
    jp      just_ret              ; (bitmap_box)
    jp      just_ret              ; (bitmap_move)
    jp      just_ret              ; (bitmap_setcell)
    jp      just_ret
    jp      just_ret
    jp      bitmap_setpixel       ; Draw pixel
    jp      bitmap_resetpixel     ; Erase pixel on 1 bpp bitmap screen
    jp      bitmap_getpixel       ; Return pixel/bloxel at position
    jp      bitmap_get            ; [future] Read Bitmap Screen Section into Buffer
    jp      just_ret              ; (bitmap_put)
    jp      just_ret
    jp      just_ret
    jp      just_ret
    jp      colormap_fill         ; Fill 1bpp Color Map Rectangle with Byte
    jp      just_ret
    jp      just_ret
    jp      just_ret
    jp      just_ret
; color.asm <<Palette and Colors>>
    jp      palette_reset         ; Reset palette to default colors
    jp      palette_set           ; palette_set
    jp      palette_get           ; Get palette
    jp      just_ret
    jp      rgb_to_asc            ; Convert Binary RGB list to ASCII
    jp      asc_to_rgb            ; Convert ASCII RRGGBB to binary GB0R
    jp      just_ret
    jp      just_ret
;; move to nybble.asm
;;    jp      read_nybbles
;;    jp      read_hex_nybble
;;    jp      hex_to_nybble
    jp      just_ret
    jp      just_ret
    jp      just_ret
    jp      just_ret
; screen.asm <<Text Screen Read and Write>>
    jp      screen_clear_color    ; Fill Color RAM with current/default colors
    jp      just_ret
    jp      just_ret
    jp      just_ret
    jp      screen_read_byte      ; Read byte from screen
    jp      screen_write_byte     ; Write byte to screen
    jp      screen_read_string    ; Read string from Screen RAM
    jp      screen_write_string   ; Write string to Screen RAM
    jp      just_ret
    jp      just_ret
    jp      screen_read_paged
    jp      screen_write_paged
    jp      screen_read_fast
    jp      screen_write_fast
    jp      just_ret
    jp      just_ret
    jp      oolor_read_byte       ; Read byte from Color RAM
    jp      color_write_byte      ; Write byte to Color RAM
    jp      color_read_string     ; Read string from Color RAM
    jp      color_write_string    ; Write string to Color RAM
    jp      just_ret
    jp      just_ret
    jp      just_ret
    jp      just_ret
; screen_gfx.asm
    jp      screen_clear_cursor   ; Remove cursor from screen
    jp      just_ret
    jp      screen_invert_color
    jp      screen_invert_cursor
    jp      just_ret
    jp      just_ret
    jp      screen_get            ; Read Text Screen Section into Buffer
    jp      screen_put            ; Write Text Screen Section from Buffer
    jp      screen_fill           ; Fill Text or Color Screen Rectangle with Byte
    jp      just_ret
    jp      just_ret
    jp      just_ret
; move to nybble.asm
;   jp      swap_nybbles
;   jp      swap_nybbles_de
; screen_swap.asm <<Text Screen Switch and Swap>>
    jp      init_screen_buffers
    jp      init_screen_vars
    jp      just_ret
    jp      just_ret
    jp      screen_reset
    jp      screen_status
    jp      just_ret
    jp      just_ret
    jp      screen_restore
    jp      screen_stash
    jp      screen_swap
    jp      screen_switch
    jp      just_ret
    jp      just_ret
; tile.asm <<Tilemap>>
    jp      tile_set
    jp      tile_get
    jp      tile_from_chrrom
    jp      tile_combine_props
    jp      just_ret
    jp      just_ret
    jp      tilemap_set_offset
    jp      tilemap_get_offset
    jp      tilemap_set_tile
    jp      tilemap_get_tile
    jp      tilemap_fill
    jp      tilemap_get
    jp      tilemap_put
    jp      just_ret
    jp      just_ret
    jp      just_ret
    jp      just_ret
; sprite_aux.asm
; currently subject to change
; in auxrom
    jp      sprite_define
    jp      sprite_defrect
    jp      sprite_set_attrs
    jp      sprite_get_attrs
    jp      spritle_set_pos
    jp      spritle_get_pos
    jp      spritle_string_attrs
    jp      spritle_reset
    jp      spritle_reset_all
; in extrom
    
_end_jump_table:

