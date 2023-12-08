;-----------------------------------------------------------------------------
; Graphic Routines Jump Table
;-----------------------------------------------------------------------------
gfx_jump_table
    jp      gfx_startup           
    jp      gfx_init              
    jp      palette_shift_num      
    jp      palette_set_entry      
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
