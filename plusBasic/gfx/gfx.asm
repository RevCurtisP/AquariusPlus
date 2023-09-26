;=====================================================================================
; plusBASIC Graphics Module
;=====================================================================================

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
screen_set_mode:
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
                                      
    byte    $21                   ;  24   1 0 0 00 1 Remap + Text 
    byte    $22                   ;  25   1 0 0 01 0 Remap + 64 x 32 Color Tilemap
    byte    $24                   ;  26   1 0 0 10 0 Remap + 320 x 200 Bitmap mode ON
    byte    $26                   ;  27   1 0 0 11 0 Remap + Multicolor Bitmap Mode
                                         
    byte    $29                   ;  28   1 0 1 00 1 Remap + Text    + Sprites
    byte    $2A                   ;  29   1 0 1 01 0 Remap + Tilemap + Sprites
    byte    $2C                   ;  30   1 0 1 10 0 Remap + Bitmap  + Sprites
    byte    $2E                   ;  31   1 0 1 11 0 Remap + Multi   + Sprites
                                          
    byte    $21                   ;  32   1 0 0 00 1 Remap + Text    
    byte    $23                   ;  33   1 0 0 01 1 Remap + Tilemap + BackText
    byte    $25                   ;  34   1 0 0 10 1 Remap + Bitmap  + BackText
    byte    $27                   ;  35   1 0 0 11 1 Remap + Multi   + BackText
                                           
    byte    $29                   ;  36   1 0 1 00 1 Remap + Text    + Sprites
    byte    $2B                   ;  37   1 0 1 01 1 Remap + Tilemap + BackText = Sprites
    byte    $2D                   ;  38   1 0 1 10 1 Remap + Bitmap  + BackText = Sprites
    byte    $2F                   ;  39   1 0 1 11 1 Remap + Multi   + BackText = Sprites
                                           
    byte    $21                   ;  40   1 0 0 00 1 Remap + Text    
    byte    $33                   ;  41   1 1 0 01 1 Remap + Tilemap + FrontText
    byte    $35                   ;  42   1 1 0 10 1 Remap + Bitmap  + FrontText
    byte    $37                   ;  43   1 1 0 11 1 Remap + Multi   + FrontText
                                           
    byte    $29                   ;  44   1 0 1 00 1 Remap + Text    + Sprites
    byte    $3B                   ;  45   1 1 1 01 1 Remap + Tilemap + FrontText = Sprites
    byte    $3D                   ;  46   1 1 1 10 1 Remap + Bitmap  + FrontText = Sprites
    byte    $3F                   ;  47   1 1 1 11 1 Remap + Multi   + FrontText = Sprites
