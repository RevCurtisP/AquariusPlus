;-----------------------------------------------------------------------------
; Graphic Routines Jump Table
;-----------------------------------------------------------------------------
;;ToDo: Move graphics routines into aux rom and jump table into jump_aux

gfx_jump_table
    jp      gfx_startup           
    jp      gfx_init              

    jp      bitmap_addr
    jp      bitmap_clear
    jp      bitmap_fill_byte
    jp      bitmap_getpixel
    jp      bitmap_resetpixel
    jp      bitmap_setpixel
    jp      bitmapc_addr
    jp      bitmapc_clear
    jp      bitmapc_fill_byte

    jp      palette_reset
    jp      palette_set           
    jp      palette_get           
    jp      tile_set_pixel        
    jp      tile_set              
    jp      tile_get              
    jp      spritle_set_attr      
    jp      sprite_set_attrs      
    jp      spritle_set_palette   
    jp      sprite_set_palettes   
    jp      spritle_set_tile      
    jp      sprite_set_tiles      
    jp      spritle_get_attrs     
    jp      sprite_get_attrs      
    jp      sprite_toggle         
    jp      spritle_toggle        
    jp      spritle_toggle_all    
    jp      sprite_set_pos        
    jp      spritle_set_pos       
    jp      spritle_clear         
    jp      spritle_clear_all        
_end_jump_table:    
