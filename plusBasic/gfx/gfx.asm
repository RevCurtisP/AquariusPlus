;=====================================================================================
; plusBASIC Graphics Module
;=====================================================================================


;-----------------------------------------------------------------------------
; Graphics System Variables - Offset from Page Start
;-----------------------------------------------------------------------------
SAVSCREEN40 equ $00   ; TTYPOS 
              ; $01   ; CURCHR
              ; $02   ; CURRAM

SAVSCREEN41 equ $04   ; TTYPOS
              ; $05   ; CURCHR
              ; $06   ; CURRAM

SAVSCREEN80 equ $08   ; TTYPOS
              ; $09   ; CURCHR
              ; $0A   ; CURRAM


;-----------------------------------------------------------------------------
; Graphic initialization routine
; routines are in common.asm for now
;-----------------------------------------------------------------------------



;Set Up Graphics System Variables
gfx_startup: 

;Initialize Graphics System Variables
gfx_init:     
    ret
    
