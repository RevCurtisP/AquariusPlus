;=====================================================================================
; Mouse related routines
;=====================================================================================

mouse_on:
    ret
    
mouse_off:
    ret

;-----------------------------------------------------------------------------
; Setup mouse sprite and tile data
; Input: BC: Data length
;        DE: Data address
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
mouse_set_tile:
    ld      a,d
    or      e
    call    z,_def_mouse_tile
    ld      hl,MOUSE_TILE         ; Mouse uses tile 255
    push    hl                    ; Stack = MsTile, RtnAdr
    call    tile_set              ; Write tile data
    ld      a,MOUSE_SPRITE
    pop     de                    ; DE = MsTile, RtnAdr
    push    af                    ; Stack = SprtNo, RtnAdr
    call    spritle_set_tile      
    pop     af                    ; A = SprtNo; Stack = RtnAdr
    ld      c,SPR_PRIORITY
    jp      spritle_set_attr      ; Set attributes and return
    
_def_mouse_tile:
    ld      bc,32
    ld      de,mouse_def_tile
    ret

mouse_def_tile:
    byte    $FF,$F0,$00,$00
    byte    $F7,$7F,$00,$00
    byte    $F7,$77,$F0,$00
    byte    $0F,$77,$7F,$00
    byte    $00,$F7,$77,$F0
    byte    $00,$0F,$77,$7F
    byte    $00,$00,$F7,$F0
    byte    $00,$00,$0F,$00
