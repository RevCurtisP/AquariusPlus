;=====================================================================================
; plusBASIC Graphics Module
;=====================================================================================


;-----------------------------------------------------------------------------
; Graphics System Variables - Offset from Page Start
;-----------------------------------------------------------------------------
BUFSCRN40 equ $00   ; TTYPOS 
            ; $01   ; CURCHR
            ; $02   ; CURRAM

BUFSCRN41 equ $04   ; TTYPOS
            ; $05   ; CURCHR
            ; $06   ; CURRAM

BUFSCRN80 equ $08   ; TTYPOS
            ; $09   ; CURCHR
            ; $0A   ; CURRAM

SWPSCRN40 equ $0C   ; TTYPOS 
            ; $0D   ; CURCHR
            ; $0E   ; CURRAM

SWPSCRN41 equ $10   ; TTYPOS
            ; $11   ; CURCHR
            ; $12   ; CURRAM
               
SWPSCRN80 equ $14   ; TTYPOS
            ; $15   ; CURCHR
            ; $16   ; CURRAM

;-----------------------------------------------------------------------------
; Graphic initialization routine
; routines are in common.asm for now
;-----------------------------------------------------------------------------



;Set Up Graphics System Variables
gfx_startup: 

;Initialize Graphics System Variables
gfx_init:     
    ret
    
