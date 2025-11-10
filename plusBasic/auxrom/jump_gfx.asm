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
    jp      bitmap_init_vars      ; C000 Initialize bitmap system variables
    jp      bitmap_set_mode       ; C003 Set bitmap mode system variable from video control register
    jp      bitmap_read_sysvars   ; C006 Read bitmap system variables
    jp      bitmap_read_color     ; C009 Get Bitmap Draw Colors
    jp      bitmap_write_color    ; C00C Set Bitmap Draw Colors
    jp      bitmap_clear_screen   ; C00F Clear Bitmap
    jp      bitmap_fill_byte      ; C012 Fill Bitmap with Byte
    jp      bitmap_fill_color     ; C015 Fill Bitmap Color RAM
    jp      bitmap_line           ; C018 [Future]
    jp      just_ret              ; C01B (bitmap_circle)
    jp      just_ret              ; C01E (bitmap_box)
    jp      just_ret              ; C021 (bitmap_move)
    jp      just_ret              ; C024 (bitmap_setcell)
    jp      just_ret              ; C027 
    jp      just_ret              ; C02A 
    jp      bitmap_setpixel       ; C02D Draw pixel
    jp      bitmap_resetpixel     ; C030 Erase pixel on 1 bpp bitmap screen
    jp      bitmap_getpixel       ; C033 Return pixel/bloxel at position
    jp      bitmap_get            ; C036 [future] Read Bitmap Screen Section into Buffer
    jp      just_ret              ; C039 (bitmap_put)
    jp      just_ret              ; C03C (bitmap_putchar)
    jp      just_ret              ; C03F (bitmap_putstring)
    jp      just_ret              ; C042 (bitmap_putclip)
    jp      colormap_fill         ; C045 Fill 1bpp Color Map Rectangle with Byte
    jp      just_ret              ; C048 
    jp      just_ret              ; C04B 
    jp      just_ret              ; C04E 
    jp      just_ret              ; C051 
; color.asm <<Palette and Colors>>
    jp      palette_reset         ; C054 Reset palette to default colors
    jp      palette_set           ; C057 Write one or more entries to palette
    jp      palette_get           ; C05A Read entire palette
    jp      palette_get_entry     ; C05D Read single palette entry
    jp      just_ret              ; C060 
    jp      just_ret              ; C063 
    jp      just_ret              ; C066 
    jp      just_ret              ; C069 
    jp      just_ret              ; C06C 
    jp      just_ret              ; C06F 
    jp      just_ret              ; C072 
    jp      just_ret              ; C075 
; screen.asm <<Text Screen Read and Write>>
    jp      screen_clear_color    ; C078 Fill Color RAM with current/default colors
    jp      just_ret              ; C07B 
    jp      just_ret              ; C07E 
    jp      just_ret              ; C081 
    jp      screen_read_byte      ; C084 Read byte from screen
    jp      screen_write_byte     ; C087 Write byte to screen
    jp      screen_read_bytes     ; C08A Read string from Screen RAM
    jp      screen_write_bytes    ; C08D Write string to Screen RAM
    jp      just_ret              ; C090 
    jp      just_ret              ; C093 
    jp      screen_read_paged     ; C096 
    jp      screen_write_paged    ; C099 
    jp      screen_read_fast      ; C09C 
    jp      screen_write_fast     ; C09F 
    jp      just_ret              ; C0A2 
    jp      just_ret              ; C0A5 
    jp      oolor_read_byte       ; C0A8 Read byte from Color RAM
    jp      color_write_byte      ; C0AB Write byte to Color RAM
    jp      color_read_bytes      ; C0AE Read string from Color RAM
    jp      color_write_bytes     ; C0B1 Write string to Color RAM
    jp      just_ret              ; C0B4 
    jp      just_ret              ; C0B7 
    jp      just_ret              ; C0BA 
    jp      just_ret              ; C0BD 
; screen_gfx.asm
    jp      screen_clear_cursor   ; C0C0 Remove cursor from screen
    jp      just_ret              ; C0C3 
    jp      screen_invert_color   ; C0C6 
    jp      screen_invert_cursor  ; C0C9 
    jp      just_ret              ; C0CC 
    jp      just_ret              ; C0CF 
    jp      screen_get            ; C0D2 Read Text Screen Section into Buffer
    jp      screen_put            ; C0D5 Write Text Screen Section from Buffer
    jp      screen_fill           ; C0D8 Fill Text or Color Screen Rectangle with Byte
    jp      just_ret              ; C0DB 
    jp      screen_mode           ; C0DE Return current Text Screen 
    jp      just_ret              ; C0E1 
; screen_swap.asm <<Text Screen Switch and Swap>>
    jp      init_screen_buffers   ; C0E4 
    jp      init_screen_vars      ; C0E7 
    jp      just_ret              ; C0EA 
    jp      just_ret              ; C0ED 
    jp      screen_reset          ; C0F0 
    jp      screen_status         ; C0F3 
    jp      just_ret              ; C0F6 
    jp      just_ret              ; C0F9 
    jp      screen_restore        ; C0FC 
    jp      screen_stash          ; C0FF 
    jp      screen_swap           ; C102 
    jp      screen_switch         ; C105 
    jp      just_ret              ; C108 
    jp      just_ret              ; C10B 
; tile.asm <<Tilemap>>
    jp      tile_set              ; C10E 
    jp      tile_get              ; C111 
    jp      tile_from_chrrom      ; C114 
    jp      tile_combine_props    ; C117 
    jp      just_ret              ; C11A 
    jp      just_ret              ; C11D 
    jp      tilemap_set_offset    ; C120 
    jp      tilemap_get_offset    ; C123 
    jp      tilemap_set_tile      ; C126 
    jp      tilemap_get_tile      ; C129 
    jp      tilemap_fill          ; C12C 
    jp      tilemap_get           ; C12F 
    jp      tilemap_put           ; C132 
    jp      just_ret              ; C135 
    jp      just_ret              ; C138 
    jp      just_ret              ; C13B 
    jp      just_ret              ; C13E 
; sprite_aux.asm <<Sprite Definition and Manipulation>>
    jp      sprite_define         ; C141 
    jp      sprite_defrect        ; C144 
    jp      sprite_set_attrs      ; C147 
    jp      sprite_get_attrs      ; C14A 
    jp      spritle_set_pos       ; C14D 
    jp      spritle_get_pos       ; C150 
    jp      spritle_set_props     ; C153 
    jp      spritle_get_props     ; C153 
    jp      spritle_string_attrs  
    jp      spritle_reset         ; C156 
    jp      spritle_reset_all     ; C159 
; in extrom
    
_end_jump_table:

