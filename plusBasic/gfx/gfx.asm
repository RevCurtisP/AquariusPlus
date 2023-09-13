;=====================================================================================
; plusBASIC Graphics Module
;=====================================================================================

;-----------------------------------------------------------------------------
; Graphic Routines Jump Table
;-----------------------------------------------------------------------------

    jp      gfx_startup           ; $8000
    jp      gfx_init              ; $8003
    jp      gfx_set_screen_mode   ; $8006 
    jp      gfx_shift_palette_num ; $8009 
    jp      gfx_set_palette_entry ; $800C 
    jp      gfx_set_palette       ; $800F
    jp      gfx_get_palette       ; $8012
    jp      gfx_set_tile_pixel    ; $8015
    jp      gfx_set_tile          ; $8018
    jp      gfx_get_tile          ; $801B
    jp      gfx_spritle_set_attr  ; $801E
    jp      gfx_sprite_set_attrs  ; $8021
    jp      gfx_spritle_set_color ; $8024
    jp      gfx_sprite_set_colors ; $8027
    jp      gfx_spritle_set_tile  ; $802A
    jp      gfx_sprite_set_tiles  ; $802D
    jp      gfx_spritle_get_attrs ; $8030
    jp      gfx_sprite_get_attrs  ; $8033
    jp      gfx_sprite_toggle     ; $8036
    jp      gfx_spritle_toggle    ; $8039
    jp      gfx_sprite_set_pos    ; $803C
    jp      gfx_spritle_set_pos   ; $803F

;-----------------------------------------------------------------------------
; Graphic initialization routine
; routines are in common.asm for now
;-----------------------------------------------------------------------------

;Set Up Graphics System Variables
gfx_startup: 

;Initialize Graphics System Variables
gfx_init:     
    ret
    
;-----------------------------------------------------------------------------
; Set Screen Mode
; Input: A: Mode (see SCREEN statement)
; Output: A: Byte written to Video Control Regiter
;        BC: Mode
;        HL: Address in lookup table
;-----------------------------------------------------------------------------
gfx_set_screen_mode:
    ld      hl,_screen_modes
    ld      b,0
    ld      c,a
    add     hl,bc
    ld      a,(hl)
    out     (IO_VCTRL),a
    ret
    
_screen_modes:                    ; mode  b p s gm t
    byte    $01                   ;   0   0 0 0 00 1 Text 
    byte    $02                   ;   1   0 0 0 01 0 64 x 32 Color Tilemap
    byte    $04                   ;   2   0 0 0 10 0 320 x 200 Bitmap mode ON
    byte    $06                   ;   3   0 0 0 11 0 Multicolor Bitmap Mode
                                           
    byte    $09                   ;   4   0 0 1 00 1 Text    + Sprites
    byte    $0A                   ;   5   0 0 1 01 0 Tilemap + Sprites
    byte    $0C                   ;   6   0 0 1 10 0 Bitmap  + Sprites
    byte    $0E                   ;   7   0 0 1 11 0 Multi   + Sprites
                                          
    byte    $01                   ;   8   0 0 0 00 1 Text    
    byte    $03                   ;   9   0 0 0 01 1 Tilemap + BackText
    byte    $05                   ;  10   0 0 0 10 1 Bitmap  + BackText
    byte    $07                   ;  11   0 0 0 11 1 Multi   + BackText
                                           
    byte    $09                   ;  12   0 0 1 00 1 Text    + Sprites
    byte    $0B                   ;  13   0 0 1 01 1 Tilemap + BackText = Sprites
    byte    $0D                   ;  14   0 0 1 10 1 Bitmap  + BackText = Sprites
    byte    $0F                   ;  15   0 0 1 11 1 Multi   + BackText = Sprites
                                           
    byte    $01                   ;  16   0 0 0 00 1 Text    
    byte    $13                   ;  17   0 1 0 01 1 Tilemap + FrontText
    byte    $15                   ;  18   0 1 0 10 1 Bitmap  + FrontText
    byte    $17                   ;  19   0 1 0 11 1 Multi   + FrontText
                                           
    byte    $09                   ;  20   0 0 1 00 1 Text    + Sprites
    byte    $1B                   ;  21   0 1 1 01 1 Tilemap + FrontText = Sprites
    byte    $1D                   ;  22   0 1 1 10 1 Bitmap  + FrontText = Sprites
    byte    $1F                   ;  23   0 1 1 11 1 Multi   + FrontText = Sprites
                                      
    byte    $01                   ;  24   1 0 0 00 1 Text 
    byte    $02                   ;  25   1 0 0 01 0 64 x 32 Color Tilemap
    byte    $04                   ;  26   1 0 0 10 0 320 x 200 Bitmap mode ON
    byte    $06                   ;  27   1 0 0 11 0 Multicolor Bitmap Mode
                                         
    byte    $09                   ;  28   1 0 1 00 1 Text    + Sprites
    byte    $0A                   ;  29   1 0 1 01 0 Tilemap + Sprites
    byte    $0C                   ;  30   1 0 1 10 0 Bitmap  + Sprites
    byte    $0E                   ;  31   1 0 1 11 0 Multi   + Sprites
                                          
    byte    $01                   ;  32   1 0 0 00 1 Text    
    byte    $03                   ;  33   1 0 0 01 1 Tilemap + BackText
    byte    $05                   ;  34   1 0 0 10 1 Bitmap  + BackText
    byte    $07                   ;  35   1 0 0 11 1 Multi   + BackText
                                           
    byte    $09                   ;  36   1 0 1 00 1 Text    + Sprites
    byte    $0B                   ;  37   1 0 1 01 1 Tilemap + BackText = Sprites
    byte    $0D                   ;  38   1 0 1 10 1 Bitmap  + BackText = Sprites
    byte    $0F                   ;  39   1 0 1 11 1 Multi   + BackText = Sprites
                                           
    byte    $01                   ;  40   1 0 0 00 1 Text    
    byte    $13                   ;  41   1 1 0 01 1 Tilemap + FrontText
    byte    $15                   ;  42   1 1 0 10 1 Bitmap  + FrontText
    byte    $17                   ;  43   1 1 0 11 1 Multi   + FrontText
                                           
    byte    $09                   ;  44   1 0 1 00 1 Text    + Sprites
    byte    $1B                   ;  45   1 1 1 01 1 Tilemap + FrontText = Sprites
    byte    $1D                   ;  46   1 1 1 10 1 Bitmap  + FrontText = Sprites
    byte    $1F                   ;  47   1 1 1 11 1 Multi   + FrontText = Sprites
